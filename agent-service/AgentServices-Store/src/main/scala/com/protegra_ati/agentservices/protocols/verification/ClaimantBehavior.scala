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
        val agntClmnt2GLoSRd =
          acT.AgentCnxn( clmnt2GLoS.src, clmnt2GLoS.label, clmnt2GLoS.trgt )
        val agntClmnt2GLoSWr =
          acT.AgentCnxn( clmnt2GLoS.trgt, clmnt2GLoS.label, clmnt2GLoS.src )
        reset {
          for( eInitiateClaim <- node.subscribe( agntClmnt2GLoSRd )( InitiateClaim.toLabel ) ) {
            rsrc2V[VerificationMessage]( eInitiateClaim ) match {
              case Left( InitiateClaim( sidIC, cidIC, vrfrIC, rpIC, clmIC ) ) => { 
                val agntVrfrWr =
                  acT.AgentCnxn( vrfrIC.src, vrfrIC.label, vrfrIC.trgt )
                val agntVrfrRd =
                  acT.AgentCnxn( vrfrIC.trgt, vrfrIC.label, vrfrIC.src )
                node.publish( agntVrfrWr )( 
                  AllowVerification.toLabel( sidIC ), 
                  AllowVerification( sidIC, cidIC, rpIC, clmIC )
                )
                for( eAllowV <- node.subscribe( agntVrfrRd )( AckAllowVerification.toLabel( sidIC ) ) ) {
                  rsrc2V[VerificationMessage]( eInitiateClaim ) match {
                    case Left( AckAllowVerification( sidAAV, cidAAV, rpAAV, clmAAV ) ) => { 
                      if (
                        sidAAV.equals( sidIC ) && cidAAV.equals( cidIC )
                        && rpAAV.equals( rpIC ) && clmAAV.equals( clmIC )
                      ) {
                        val agntRPWr =
                          acT.AgentCnxn( rpIC.src, rpIC.label, rpIC.trgt )
                        val agntRPRd =
                          acT.AgentCnxn( rpIC.trgt, rpIC.label, rpIC.src )
                        reset {
                          node.publish( agntRPWr )( 
                            OpenClaim.toLabel( sidIC ), 
                            OpenClaim( sidIC, cidIC, vrfrIC, clmIC )
                          )            
                        }
                        for( eCloseClaim <- node.subscribe( agntRPRd )( CloseClaim.toLabel( sidIC ) ) ) {
                          rsrc2V[VerificationMessage]( eCloseClaim ) match {
                            case Left( CloseClaim( sidCC, cidCC, vrfrCC, clmCC, witCC ) ) => { 
                              if (
                                sidCC.equals( sidIC ) && cidCC.equals( cidIC )
                                && vrfrCC.equals( vrfrIC ) && clmCC.equals( clmIC )
                              ) {
                                  node.publish( agntClmnt2GLoSWr )(
                                    CompleteClaim.toLabel( sidIC ),
                                    CompleteClaim( sidCC, cidCC, vrfrCC, clmCC, witCC )
                                  )
                              }
                              else {
                                BasicLogService.tweet( "close doesn't match open : " + eCloseClaim )
                                node.publish( agntClmnt2GLoSWr )(
                                  CompleteClaim.toLabel( sidIC ),
                                  CompleteClaim(
                                    sidIC, cidIC, vrfrIC, clmIC,
                                    "protocolError(\"relying party close claim does not match open claim\")".toLabel
                                  )
                                )
                              }
                            }
                            case Right( true ) => {
                              BasicLogService.tweet( "waiting for close claim" )
                            }
                            case _ => {
                              // BUGBUG : lgm -- protect against strange and
                              // wondrous toString implementations (i.e. injection
                              // attack ) for eInitiateClaim
                              BasicLogService.tweet( "unexpected protocol message : " + eCloseClaim )
                              node.publish( agntClmnt2GLoSWr )(
                                VerificationNotification.toLabel( sidIC ),
                                VerificationNotification(
                                  sidIC, cidIC, clmnt2GLoS, clmIC,
                                  (
                                    "protocolError(\"unexpected protocol message\","
                                    + "\"" + eCloseClaim + "\"" + ")"
                                  ).toLabel
                                )
                              )
                            }
                          }
                        }
                      }
                      else {
                        BasicLogService.tweet( "ack doesn't match : " + eAllowV )
                        node.publish( agntClmnt2GLoSWr )(
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
                      node.publish( agntClmnt2GLoSWr )(
                        VerificationNotification.toLabel( sidIC ),
                        VerificationNotification(
                          sidIC, cidIC, clmnt2GLoS, clmIC,
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
              case Right( true ) => {
                BasicLogService.tweet( "waiting for claim initiation" )
              }
              case _ => {
                // BUGBUG : lgm -- protect against strange and
                // wondrous toString implementations (i.e. injection
                // attack ) for eInitiateClaim
                BasicLogService.tweet( "unexpected protocol message : " + eInitiateClaim )
                node.publish( agntClmnt2GLoSWr )(
                  VerificationNotification.toLabel(),
                  VerificationNotification(
                    null, null, null, null,
                    (
                      "protocolError(\"unexpected protocol message\","
                      + "\"" + eInitiateClaim + "\"" + ")"
                    ).toLabel
                  )
                )
              }
            }
          }
        }
      }
      case _ => {
        BasicLogService.tweet( "one cnxn expected : " + cnxns )
        throw new Exception( "one cnxn expected : " + cnxns )
      }
    }
  }
}
