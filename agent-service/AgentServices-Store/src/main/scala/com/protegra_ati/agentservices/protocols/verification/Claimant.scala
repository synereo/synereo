// -*- mode: Scala;-*- 
// Filename:    Claimant.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 11 11:00:15 2014 
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

trait ClaimantBehaviorT extends Serializable {
  import com.biosimilarity.evaluator.distribution.utilities.DieselValueTrampoline._
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  def run(
    node : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns : Seq[PortableAgentCnxn],
    filters : Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    doVerification( node, cnxns )
  }  
  def doVerification(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn]
  ): Unit = {    
    cnxns match {
      case clmnt2GLoS :: Nil => {
        val agntClmnt2GLoS =
          acT.AgentCnxn( clmnt2GLoS.src, clmnt2GLoS.label, clmnt2GLoS.trgt )
        reset {
          for( eInitiateClaim <- node.subscribe( agntClmnt2GLoS )( InitiateClaim.toLabel ) ) {
            rsrc2V[VerificationMessage]( eInitiateClaim ) match {
              case Left( InitiateClaim( sidIC, cidIC, vrfrIC, rpIC, clmIC ) ) => { 
                val agntVrfr =
                  acT.AgentCnxn( vrfrIC.src, vrfrIC.label, vrfrIC.trgt )
                node.publish( agntVrfr )( 
                  AllowVerification.toLabel( sidIC ), 
                  AllowVerification( sidIC, cidIC, rpIC, clmIC )
                )
                for( eAllowV <- node.subscribe( agntVrfr )( AckAllowVerification.toLabel( sidIC ) ) ) {
                  rsrc2V[VerificationMessage]( eInitiateClaim ) match {
                    case Left( AckAllowVerification( sidAAV, cidAAV, rpAAV, clmAAV ) ) => { 
                      if (
                        sidAAV.equals( sidIC ) && cidAAV.equals( cidIC )
                        && rpAAV.equals( rpIC ) && clmAAV.equals( clmIC )
                      ) {
                        val agntRP =
                          acT.AgentCnxn( rpIC.src, rpIC.label, rpIC.trgt )
                        reset {
                          node.publish( agntRP )( 
                            OpenClaim.toLabel( sidIC ), 
                            OpenClaim( sidIC, cidIC, rpIC, clmIC )
                          )            
                        }
                        for( eCloseClaim <- node.subscribe( agntRP )( CloseClaim.toLabel( sidIC ) ) ) {
                          rsrc2V[VerificationMessage]( eCloseClaim ) match {
                            case Left( CloseClaim( sidCC, cidCC, vrfrCC, clmCC, dataCC ) ) => {                               
                              if (
                                sidCC.equals( sidIC ) && cidCC.equals( cidIC )
                                && vrfrCC.equals( vrfrIC ) && clmCC.equals( clmIC )
                              ) {
                                  node.publish( agntClmnt2GLoS )(
                                    CompleteClaim.toLabel( sidIC ),
                                    CompleteClaim( sidCC, cidCC, vrfrCC, clmCC, dataCC )
                                  )
                              }
                              else {
                                BasicLogService.tweet( "close doesn't match open : " + eCloseClaim )
                                node.publish( agntClmnt2GLoS )(
                                  CompleteClaim.toLabel( sidIC ),
                                  CompleteClaim(
                                    sidIC, cidIC, vrfrIC, clmIC,
                                    "protocolError(\"relying party close claim does not match open claim\")".toLabel
                                  )
                                )
                                //throw new Exception( "close doesn't match open : " + eCloseClaim )
                              }
                            }
                          }
                        }
                      }
                      else {
                        BasicLogService.tweet( "ack doesn't match : " + eAllowV )
                        node.publish( agntClmnt2GLoS )(
                          CompleteClaim.toLabel( sidIC ),
                          CompleteClaim(
                            sidIC, cidIC, vrfrIC, clmIC,
                            "protocolError(\"verifier acknowledgment does not match allow verification\")".toLabel
                          )
                        )
                      }
                    }
                    case Right( true ) => {
                      BasicLogService.tweet( "waiting for allow claim ack" )
                    }
                    case _ => {
                      BasicLogService.tweet( "unexpected protocol message : " + eAllowV )
                      throw new Exception( "unexpected protocol message : " + eAllowV )
                    }
                  }
                }
              }
              case Right( true ) => {
                BasicLogService.tweet( "waiting for claim initiation" )
              }
              case _ => {
                BasicLogService.tweet( "unexpected protocol message : " + eInitiateClaim )
                throw new Exception( "unexpected protocol message : " + eInitiateClaim )
              }
            }
          }
        }
      }
      case _ => {
        BasicLogService.tweet( "wrong number of cnxns : " + cnxns )
        throw new Exception( "wrong number of cnxns : " + cnxns )
      }
    }
  }
}
