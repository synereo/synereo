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
    doVerification( node, cnxns )
  }

  def doVerification(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn]
  ): Unit = {
    val vrfr2GLoS :: agntCnxns =
      cnxns.map( { cnxn : PortableAgentCnxn => acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt ) } )
    for( cnxn <- agntCnxns ) {
      reset {
        for( eAllowV <- node.subscribe( cnxn )( AllowVerification.toLabel ) ) {
          rsrc2V[VerificationMessage]( eAllowV ) match {
            case Left( AllowVerification( sidAV, cidAV, rpAV, clmAV ) ) => { 
              val agntRP =
                acT.AgentCnxn( rpAV.src, rpAV.label, rpAV.trgt )
              node.publish( cnxn )( // BUGBUG : lgm -- needs to be the dual cnxn
                AckAllowVerification.toLabel( sidAV ),
                mTT.Ground( PostedExpr( AckAllowVerification( sidAV, cidAV, rpAV, clmAV ) ) )
              )
              for( eVerify <- node.subscribe( agntRP )( Verify.toLabel ) ) {
                rsrc2V[VerificationMessage]( eVerify ) match {
                  case Left( Verify( sidV, cidV, clmntV, clmV ) ) => {                     
                    if (
                      sidV.equals( sidAV ) && cidV.equals( cidAV )
                      && clmV.equals( clmAV )
                    ) {
                      node.publish( cnxn )(
                        Verification.toLabel( sidAV ),
                        Verification( sidV, cidV, clmntV, clmV, "claimVerified( true )".toLabel )
                      )
                      node.publish( vrfr2GLoS )(
                        VerificationNotification.toLabel( sidAV ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV,
                          "claimVerified( true )".toLabel
                        )
                      )
                    }
                    else {
                      node.publish( cnxn )(
                        Verification.toLabel( sidAV ),
                        Verification(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verify message data\")".toLabel )
                      )
                      node.publish( vrfr2GLoS )(
                        VerificationNotification.toLabel( sidAV ),
                        VerificationNotification(
                          sidV, cidV, clmntV, clmV,
                          "protocolError(\"unexpected verify message data\")".toLabel
                        )
                      )
                    }
                  }
                }
              }
            }
            case Right( true ) => {
              BasicLogService.tweet( "waiting for claim initiation" )
            }
            case _ => {
              BasicLogService.tweet( "unexpected protocol message : " + eAllowV )
              throw new Exception( "unexpected protocol message : " + eAllowV )
            }
          }
        }
      }
    }
  }
}
