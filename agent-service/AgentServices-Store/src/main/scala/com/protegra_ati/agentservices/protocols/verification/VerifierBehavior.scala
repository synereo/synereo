// -*- mode: Scala;-*- 
// Filename:    VerifierBehavior.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 27 10:29:48 2014 
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

trait VerifierBehaviorT extends Serializable {
  import com.biosimilarity.evaluator.distribution.utilities.DieselValueTrampoline._
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  def run(
    node : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns : Seq[PortableAgentCnxn],
    filters : Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    // BUGBUG : lgm -- move defensive check on args to run method
    doVerification( node, cnxns )
  }

  def doVerification(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn]
  ): Unit = {
    val vrfr2GLoSWr :: agntCnxn :: rAgntCnxns =
      cnxns.map( { cnxn : PortableAgentCnxn => acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt ) } )
    val agntCnxns = agntCnxn :: rAgntCnxns

    for( cnxnRd <- agntCnxns ) {
      val cnxnWr = acT.AgentCnxn( cnxnRd.src, cnxnRd.label, cnxnRd.trgt )
      reset {
        for( eAllowV <- node.subscribe( cnxnRd )( AllowVerification.toLabel ) ) {
          rsrc2V[VerificationMessage]( eAllowV ) match {
            case Left( AllowVerification( sidAV, cidAV, rpAV, clmAV ) ) => { 
              val agntRPRd =
                acT.AgentCnxn( rpAV.src, rpAV.label, rpAV.trgt )
              val agntRPWr =
                acT.AgentCnxn( rpAV.trgt, rpAV.label, rpAV.src )

              node.publish( cnxnWr )( 
                AckAllowVerification.toLabel( sidAV ),
                mTT.Ground( PostedExpr( AckAllowVerification( sidAV, cidAV, rpAV, clmAV ) ) )
              )
              for( eVerify <- node.subscribe( agntRPRd )( Verify.toLabel ) ) {
                rsrc2V[VerificationMessage]( eVerify ) match {
                  case Left( Verify( sidV, cidV, clmntV, clmV ) ) => { 
                    if (
                      sidV.equals( sidAV ) && cidV.equals( cidAV )
                      && clmV.equals( clmAV ) // BUGBUG : lgm -- this is only a first
                                              // approximation. We can
                                              // use the full weight
                                              // of prolog here to
                                              // prove the claim.
                    ) {
                      node.publish( agntRPWr )(
                        Verification.toLabel( sidAV ),
                        Verification( sidV, cidV, clmntV, clmV, "claimVerified( true )".toLabel )
                      )
                      node.publish( vrfr2GLoSWr )(
                        VerificationNotification.toLabel( sidAV ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV,
                          "claimVerified( true )".toLabel
                        )
                      )
                    }
                    else {
                      node.publish( agntRPWr )(
                        Verification.toLabel( sidAV ),
                        Verification(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verify message data\")".toLabel 
                        )
                      )
                      node.publish( vrfr2GLoSWr )(
                        VerificationNotification.toLabel( sidAV ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verify message data\")".toLabel
                        )
                      )
                    }
                  }
                  case Right( true ) => {
                    BasicLogService.tweet( "waiting for verify request" )
                  }
                  case _ => {
                    val paCnxnRd =
                      PortableAgentCnxn( cnxnRd.src, cnxnRd.label, cnxnRd.trgt )
                    BasicLogService.tweet( "unexpected protocol message : " + eVerify )
                    node.publish( vrfr2GLoSWr )(
                      VerificationNotification.toLabel(),
                      VerificationNotification(
                        sidAV, cidAV, paCnxnRd, clmAV,
                        (
                          "protocolError(\"unexpected protocol message\","
                          + "\"" + eVerify + "\"" + ")"
                        ).toLabel
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
              BasicLogService.tweet( "unexpected protocol message : " + eAllowV )
              node.publish( vrfr2GLoSWr )(
                VerificationNotification.toLabel(),
                VerificationNotification(
                  null, null, null, null,
                  (
                    "protocolError(\"unexpected protocol message\","
                    + "\"" + eAllowV + "\"" + ")"
                  ).toLabel
                )
              )
            }
          }
        }
      }
    }
  }
}
