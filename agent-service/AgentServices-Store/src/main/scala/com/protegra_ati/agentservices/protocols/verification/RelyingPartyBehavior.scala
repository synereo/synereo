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

trait RelyingPartyBehaviorT extends ProtocolBehaviorT with Serializable {
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
      BasicLogService.tweet(
        (
          "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          + "\nwaiting for open claim request on: " 
          + "\ncnxn: " + cnxnRd
          + "\nlabel: " + OpenClaim.toLabel
          + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        )
      )    
      reset {
        for( eOpenClaim <- node.subscribe( cnxnRd )( OpenClaim.toLabel ) ) {
          rsrc2V[VerificationMessage]( eOpenClaim ) match {
            case Left( OpenClaim( sidOC, cidOC, vrfrOC, clmOC ) ) => { 
              BasicLogService.tweet(
                (
                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  + "\nreceived open claim request: " + eOpenClaim
                  + "\ncnxn: " + cnxnRd
                  + "\nlabel: " + OpenClaim.toLabel
                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                )
              )

              val agntVrfrRd =
                acT.AgentCnxn( vrfrOC.src, vrfrOC.label, vrfrOC.trgt )
              val agntVrfrWr =
                acT.AgentCnxn( vrfrOC.trgt, vrfrOC.label, vrfrOC.src )
              val verifyRq = 
                Verify( sidOC, cidOC, pacCnxnRd, clmOC )

              BasicLogService.tweet(
                (
                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  + "\npublishing verify request: " + verifyRq
                  + "\n on cnxn: " + agntVrfrWr
                  + "\n label: " + Verify.toLabel( sidOC )
                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                )
              )

              node.publish( agntVrfrWr )( 
                Verify.toLabel( sidOC ),
                verifyRq
              )

              BasicLogService.tweet(
                (
                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  + "\nwaiting for verification testimony on: " 
                  + "\ncnxn: " + agntVrfrRd
                  + "\nlabel: " + Verification.toLabel
                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                )
              )    

              for( eVerification <- node.subscribe( agntVrfrRd )( Verification.toLabel ) ) {
                rsrc2V[VerificationMessage]( eVerification ) match {
                  case Left( Verification( sidV, cidV, clmntV, clmV, witV ) ) => { 
                    BasicLogService.tweet(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nreceived verification testimony: " + eVerification
                        + "\ncnxn: " + agntVrfrRd
                        + "\nlabel: " + Verification.toLabel
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
                    if (
                      sidV.equals( sidOC ) && cidV.equals( cidOC )
                      && clmV.equals( clmOC )
                    ) {
                      val closeClaim =
                        CloseClaim( sidV, cidV, clmntV, clmV, witV )
                      val notification =
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV, witV
                        )
                    
                      BasicLogService.tweet(
                        (
                          "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          + "\nverification testimony matches request parameters" 
                          + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        )
                      )

                      BasicLogService.tweet(
                        (
                          "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          + "\npublishing close claim: " + closeClaim
                          + "\n on cnxn: " + cnxnWr
                          + "\n label: " + closeClaim
                          + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        )
                      )

                      node.publish( cnxnWr )( 
                        CloseClaim.toLabel( sidOC ),
                        closeClaim
                      )
                      
                      BasicLogService.tweet(
                        (
                          "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          + "\npublishing VerificationNotification: " + notification
                          + "\n on cnxn: " + rp2GLoSWr
                          + "\n label: " + VerificationNotification.toLabel( sidOC )
                          + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        )
                      )

                      node.publish( rp2GLoSWr )(
                        VerificationNotification.toLabel( sidOC ),
                        notification
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
                    BasicLogService.tweet(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nstill waiting for verification from verifier"
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
                  }
                  case _ => {
                    BasicLogService.tweet(
                      (
                        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                        + "\nunexpected protocol message : " + eVerification
                        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      )
                    )
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
              BasicLogService.tweet(
                (
                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  + "\nstill waiting for open claim request from relying party"
                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                )
              )
            }
            case _ => {
              BasicLogService.tweet(
                (
                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                  + "\nunexpected protocol message : " + eOpenClaim
                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                )
              )
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
