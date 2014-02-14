// -*- mode: Scala;-*- 
// Filename:    Driver.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 13 16:40:06 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.{PortableAgentCnxn, PortableAgentBiCnxn}
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.protegra_ati.agentservices.protocols.msgs._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import java.util.UUID

package usage {  
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
  import com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel

  import com.biosimilarity.evaluator.distribution.utilities.DieselValueTrampoline._

  import com.biosimilarity.evaluator.distribution.NodeStreams
  import com.biosimilarity.evaluator.distribution.FuzzyStreams
  import com.biosimilarity.evaluator.distribution.FuzzyTerms
  import com.biosimilarity.evaluator.distribution.FuzzyTermStreams

  import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  import com.biosimilarity.lift.model.store.CnxnString
  import scala.concurrent.FJTaskRunnersX

  trait GLoSStubT
    extends NodeStreams
     with FuzzyStreams
     with FuzzyTerms
     with FuzzyTermStreams
     with CnxnString[String,String,String] 
     with Serializable
  {
    def claimantToGLoS : PortableAgentCnxn
    def verifierToGLoS : PortableAgentCnxn
    def relyingPartyToGLoS : PortableAgentCnxn
    def waitForSignalToInitiateClaim(
      continuation : CnxnCtxtLabel[String,String,String] => Unit
    ) : Unit = {     
      println( "Please state your claim: " )
      val ln = readLine() // Note: this is blocking.
      val claim = ln.toLabel
      continuation( claim )
      println( "your claim, " + claim + ", has been submitted." )
     }
    def waitForVerifierVerificationNotification(
      node : StdEvalChannel,      
      continuation : VerificationMessage => Unit
    ) : Unit = {
      val vrfr2GLoSRd =
        acT.AgentCnxn( verifierToGLoS.src, verifierToGLoS.label, verifierToGLoS.trgt )
      reset {
        for(
          eVNote <- node.subscribe(
            vrfr2GLoSRd
          )( VerificationNotification.toLabel() )
        ) {
          rsrc2V[VerificationMessage]( eVNote ) match {
            case Left( vmsg@VerificationNotification( sidVN, cidVN, clmntVN, clmVN, witVN ) ) => { 
              BasicLogService.tweet( "received verification notification " + eVNote )
              continuation( vmsg )
            }
            case Right( true ) => {
              BasicLogService.tweet( "waiting for claim initiation" )
            }
            case _ => {
              BasicLogService.tweet( "unexpected protocol message : " + eVNote )
            }
          }
        }
      }
    }
    def waitForRelyingPartyVerificationNotification(
      node : StdEvalChannel,
      continuation : VerificationMessage => Unit
    ) : Unit = {
      val rp2GLoSRd =
        acT.AgentCnxn( relyingPartyToGLoS.src, relyingPartyToGLoS.label, relyingPartyToGLoS.trgt )
      reset {
        for(
          eVNote <- node.subscribe(
            rp2GLoSRd
          )( VerificationNotification.toLabel() )
        ) {
          rsrc2V[VerificationMessage]( eVNote ) match {
            case Left( vmsg@VerificationNotification( sidVN, cidVN, clmntVN, clmVN, witVN ) ) => { 
              BasicLogService.tweet( "received verification notification " + eVNote )
              continuation( vmsg )
            }
            case Right( true ) => {
              BasicLogService.tweet( "waiting for claim initiation" )
            }
            case _ => {
              BasicLogService.tweet( "unexpected protocol message : " + eVNote )
            }
          }
        }
      }
    }
    def waitForCompleteClaim(
      node : StdEvalChannel,
      continuation : VerificationMessage => Unit
    ) : Unit = {
      val clmnt2GLoSRd =
        acT.AgentCnxn( claimantToGLoS.src, claimantToGLoS.label, claimantToGLoS.trgt )
      reset {
        for(
          eCompleteClaim <- node.subscribe(
            clmnt2GLoSRd
          )( CloseClaim.toLabel() )
        ) {
          rsrc2V[VerificationMessage]( eCompleteClaim ) match {
            case Left( vmsg@CompleteClaim( sidCC, cidCC, vrfrCC, clmCC, witCC ) ) => { 
              BasicLogService.tweet( "received verification notification " + eCompleteClaim )
              continuation( vmsg )
            }
            case Right( true ) => {
              BasicLogService.tweet( "waiting for claim initiation" )
            }
            case _ => {
              BasicLogService.tweet( "unexpected protocol message : " + eCompleteClaim )
            }
          }
        }
      }
    }
  }    

  case class GLoSStub(
    override val claimantToGLoS : PortableAgentCnxn,
    override val verifierToGLoS : PortableAgentCnxn,
    override val relyingPartyToGLoS : PortableAgentCnxn
  ) extends GLoSStubT 

  object VerificationDriver
   extends NodeStreams
     with FuzzyStreams
     with FuzzyTerms
     with FuzzyTermStreams
     with CnxnString[String,String,String] 
     with FJTaskRunnersX
     with Serializable
  {    
    def nextClaimant() : ClaimantBehavior = {
      ClaimantBehavior()
    }
    def nextVerifier() : VerifierBehavior = {
      VerifierBehavior()
    }
    def nextRelyingParty() : RelyingPartyBehavior = {
      RelyingPartyBehavior()
    }
    def claimantStrm() : Stream[ClaimantBehavior] = {
      tStream( nextClaimant() )( { clmnt => nextClaimant() } )
    }
    def verifierStrm() : Stream[VerifierBehavior] = {
      tStream( nextVerifier() )( { vrfr => nextVerifier() } )
    }
    def relyingPartyStrm() : Stream[RelyingPartyBehavior] = {
      tStream( nextRelyingParty() )( { rp => nextRelyingParty() } )
    }    
    
    def connect(
      selfCnxn1 : PortableAgentCnxn,
      selfCnxn2 : PortableAgentCnxn
    )(
      useSrc : Boolean = true
    ): PortableAgentCnxn = {
      val cnxnLabel = UUID.randomUUID().toString
      if ( useSrc ) {
        PortableAgentCnxn( selfCnxn1.src, cnxnLabel, selfCnxn2.src )
      } 
      else {
        PortableAgentCnxn( selfCnxn1.trgt, cnxnLabel, selfCnxn2.trgt )
      }
    }

    def verificationEnsembleCnxnStrm(
    ) : Stream[(PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn)] = {
      val ( cCnxnStrm, vCnxnStrm, rCnxnStrm ) =
        ( mkSelfCnxnStream(), mkSelfCnxnStream(), mkSelfCnxnStream() );
      for(
        ( c, ( v, r ) ) <- cCnxnStrm.zip( vCnxnStrm.zip( rCnxnStrm ) )
      ) yield {
        ( c, v, r, connect( c, v )( true ), connect( c, r )( true ), connect( v, r )( true ) )
      }        
    }
    def verificationEnsembleStrm(
    ) : Stream[(ClaimantBehavior,VerifierBehavior,RelyingPartyBehavior)] = {
      for(
        ( c, ( v, r ) ) <- claimantStrm().zip( verifierStrm().zip( relyingPartyStrm() ) )
      ) yield {
        ( c, v, r )
      }
    }

    def verificationEnsembleTestStrm(
      node : StdEvalChannel,
      glosStub : GLoSStub,
      clmntBhvr : ClaimantBehavior,
      vrfrBhvr : VerifierBehavior,
      rpBhvr : RelyingPartyBehavior
    )(
      cnxnStrm : Stream[(PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn,PortableAgentCnxn)]
    ): Stream[() => Unit] = {      
      for(
        ( c, v, r, c2v, c2r, v2r ) <- cnxnStrm
      ) yield {
        val agntCRd = 
          acT.AgentCnxn( c.src, c.label, c.trgt )
        val agntCWr = 
          acT.AgentCnxn( c.trgt, c.label, c.src )

        val sid = UUID.randomUUID.toString
        val cid = UUID.randomUUID.toString

        () => {
          glosStub.waitForSignalToInitiateClaim(
            ( claim : CnxnCtxtLabel[String,String,String] ) => {
              reset {
                node.publish( agntCWr )(
                  InitiateClaim.toLabel( sid ),
                  InitiateClaim( sid, cid, c2v, c2r, claim )
                )
              }
              spawn { 
                clmntBhvr.run(
                  node,
                  List[PortableAgentCnxn](
                    glosStub.claimantToGLoS
                  ),
                  List[CnxnCtxtLabel[String, String, String]]( )
                )
              }
              spawn {
                vrfrBhvr.run(
                  node,
                  List[PortableAgentCnxn](
                    glosStub.verifierToGLoS,
                    c2v
                  ),
                  List[CnxnCtxtLabel[String, String, String]]( )
                )
              }
              spawn {
                rpBhvr.run(
                  node,
                  List[PortableAgentCnxn](
                    glosStub.relyingPartyToGLoS,
                    c2r
                  ),
                  List[CnxnCtxtLabel[String, String, String]]( )
                )
              }
            }
          )
        }                
      }
    }
  }
}
