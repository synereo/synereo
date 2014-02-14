// -*- mode: Scala;-*- 
// Filename:    RelyingPartyBehavior.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 11 10:59:14 2014 
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

trait RelyingPartyBehaviorT extends Serializable {
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
    val ( rp2GLoSRd, rp2GLoSWr, _ ) :: agntCnxns =
      cnxns.map(
        {
          ( cnxn : PortableAgentCnxn ) => {
            (
              acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt ),
              acT.AgentCnxn( cnxn.trgt, cnxn.label, cnxn.src ),
              cnxn
            )
          }
        }
      )
    for( ( cnxnRd, cnxnWr, pacCnxnRd ) <- agntCnxns ) {
      reset {
        for( eOpenClaim <- node.subscribe( cnxnRd )( OpenClaim.toLabel ) ) {
          rsrc2V[VerificationMessage]( eOpenClaim ) match {
            case Left( OpenClaim( sidOC, cidOC, vrfrOC, clmOC ) ) => { 
              val agntVrfrWr =
                acT.AgentCnxn( vrfrOC.src, vrfrOC.label, vrfrOC.trgt )
              val agntVrfrRd =
                acT.AgentCnxn( vrfrOC.trgt, vrfrOC.label, vrfrOC.src )

              node.publish( agntVrfrWr )( 
                Verify.toLabel( sidOC ),
                mTT.Ground( PostedExpr( Verify( sidOC, cidOC, pacCnxnRd, clmOC ) ) )
              )
              for( eVerification <- node.subscribe( agntVrfrRd )( Verification.toLabel ) ) {
                rsrc2V[VerificationMessage]( eVerification ) match {
                  case Left( Verification( sidV, cidV, clmntV, clmV, witV ) ) => { 
                    if (
                      sidV.equals( sidOC ) && cidV.equals( cidOC )
                      && clmV.equals( clmOC )
                    ) {
                      node.publish( cnxnWr )( 
                        CloseClaim.toLabel( sidOC ),
                        CloseClaim( sidV, cidV, clmntV, clmV, witV )
                      )
                      node.publish( rp2GLoSWr )(
                        VerificationNotification.toLabel( sidOC ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV, witV
                        )
                      )
                    }
                    else {
                      node.publish( cnxnWr )(
                        CloseClaim.toLabel( sidOC ),
                        CloseClaim(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verification message data\")".toLabel )
                      )
                      node.publish( rp2GLoSWr )(
                        VerificationNotification.toLabel( sidOC ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verify message data\")".toLabel
                        )
                      )
                    }
                  }
                  case Right( true ) => {
                    BasicLogService.tweet( "waiting for claim initiation" )
                  }
                  case _ => {
                    BasicLogService.tweet( "unexpected protocol message : " + eOpenClaim )
                    node.publish( rp2GLoSWr )(
                      VerificationNotification.toLabel(),
                      VerificationNotification(
                        sidOC, cidOC, pacCnxnRd, clmOC, 
                        "protocolError(\"unexpected verify message data\")".toLabel
                      )
                    )
                  }
                }
              }
            }
            case Right( true ) => {
              BasicLogService.tweet( "waiting for claim initiation" )
            }
            case _ => {
              BasicLogService.tweet( "unexpected protocol message : " + eOpenClaim )
              node.publish( rp2GLoSWr )(
                VerificationNotification.toLabel(),
                VerificationNotification(
                  null, null, null, null, 
                  "protocolError(\"unexpected verify message data\")".toLabel
                )
              )
            }
          }
        }
      }
    }
  }
}

case class RelyingPartyBehavior(
) extends RelyingPartyBehaviorT
