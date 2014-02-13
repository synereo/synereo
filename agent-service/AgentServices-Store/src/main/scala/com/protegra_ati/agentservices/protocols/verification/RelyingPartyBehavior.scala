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
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.protegra_ati.agentservices.protocols.msgs._
import scala.util.continuations._
import java.util.UUID

trait RelyingPartyBehaviorT extends Serializable {
  def run(
    node : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns : Seq[PortableAgentCnxn],
    filters : Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    
  }
  implicit def toAgentCnxn( cnxn : PortableAgentCnxn ) : acT.AgentCnxn = {
    acT.AgentCnxn(cnxn.src, cnxn.label, cnxn.trgt)
  }
  def doVerification(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn]
  ): Unit = {
    val agntCnxns =
      cnxns.map( { cnxn : PortableAgentCnxn => acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt ) } )
    for( cnxn <- agntCnxns ) {
      reset {
        for( eAllowV <- node.subscribe( cnxn )( AllowVerification.toLabel ) ) {
          eAllowV match {
            case Some( mTT.Ground( PostedExpr( AllowVerification( sidAV, cidAV, rpAV, clmAV ) ) ) ) => {
              node.publish( cnxn )(
                AckAllowVerification.toLabel( sidAV ),
                mTT.Ground( PostedExpr( AckAllowVerification( sidAV, cidAV, rpAV, clmAV ) ) )
              )
              for( eV <- node.subscribe( rpAV )( Verify.toLabel ) ) {
                eV match {
                  case Some( mTT.Ground( PostedExpr( Verify( sidV, cidV, clmntV, clmV ) ) ) ) => {
                    // check that cmlntV.src equals cnxn.src
                    // check that clmV equals clmAV
                  }
                  case Some( mTT.RBoundHM( Some( mTT.Ground( PostedExpr( Verify( sidV, cidV, clmntV, clmV ) ) ) ), _ ) ) => {
                  }
                  case Some( mTT.RBoundAList( Some( mTT.Ground( PostedExpr( Verify( sidV, cidV, clmntV, clmV ) ) ) ), _ ) ) => {
                  }
                }
              }
            }
            case Some( mTT.RBoundHM( Some( mTT.Ground( PostedExpr( AllowVerification( sidAV, cidAV, rpAV, clmAV ) ) ) ), _ ) ) => {
            }
            case Some( mTT.RBoundAList( Some( mTT.Ground( PostedExpr( AllowVerification( sidAV, cidAV, rpAV, clmAV ) ) ) ), _ ) ) => {
            }
          }
        }
      }
    }
  }
}
