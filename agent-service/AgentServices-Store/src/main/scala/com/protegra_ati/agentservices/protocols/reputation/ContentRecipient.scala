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

trait ContentRecipientBehaviorT extends ProtocolBehaviorT with Serializable {
  import com.biosimilarity.evaluator.distribution.utilities.DieselValueTrampoline._
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  def run(
    node : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns : Seq[PortableAgentCnxn],
    filters : Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    BasicLogService.tweet(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\ncontentRecipient -- behavior instantiated and run method invoked " 
        + "\nnode: " + node
        + "\ncnxns: " + cnxns
        + "\nfilters: " + filters
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )
    println(
      (
        "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        + "\ncontentRecipient -- behavior instantiated and run method invoked " 
        + "\nnode: " + node
        + "\ncnxns: " + cnxns
        + "\nfilters: " + filters
        + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
      )
    )
    receiveContent( node, cnxns )
  }  
  def receiveContent(
    node: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn]
  ): Unit = {    
    cnxns match {
      case recipientToSomebody :: _ => {
        val agntRecipientCnxnsRdWr =
          for( cnxn <- cnxns ) yield {
            (
              acT.AgentCnxn( cnxn.src, cnxn.label, cnxn.trgt ),
              acT.AgentCnxn( cnxn.trgt, cnxn.label, cnxn.src )
            )
          }

        for( ( agntCnxn, _ ) <- agntRecipientCnxnsRdWr ) {
          BasicLogService.tweet(
            (
              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              + "\ncontentRecipient -- waiting for content publication on: " 
              + "\nagntCnxn: " + agntCnxn
              + "\nlabel: " + PublishWithReo.toLabel
              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            )
          )
          println(
            (
              "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
              + "\nclaimant -- waiting for initiate claim on: " 
              + "\nagntCnxn: " + agntCnxn
              + "\nlabel: " + PublishWithReo.toLabel
              + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            )
          )
          
          reset {
            for( ePublishWithReo <- node.subscribe( agntCnxn )( PublishWithReo.toLabel ) ) {
              rsrc2V[ReputationMessage]( ePublishWithReo ) match {
                case Left( PublishWithReo( sidPC, cidPC, p2cPC, cntntPC, reoPC ) ) => {
                  BasicLogService.tweet(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- received PublishWithReo request: " + ePublishWithReo
                      + "\ncnxn: " + agntCnxn
                      + "\nlabel: " + PublishWithReo.toLabel
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- received PublishWithReo request: " + ePublishWithReo
                      + "\ncnxn: " + agntCnxn
                      + "\nlabel: " + PublishWithReo.toLabel
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                }
                case Right( true ) => {
                  BasicLogService.tweet(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- still waiting for PublishWithReo request"
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- still waiting for PublishWithReo request"
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                }
                case _ => {
                  // BUGBUG : lgm -- protect against strange and
                  // wondrous toString implementations (i.e. injection
                  // attack ) for eInitiateClaim
                  BasicLogService.tweet(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- while waiting for PublishWithReo request"
                      + "\nreceived unexpected protocol message : " + ePublishWithReo
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                  println(
                    (
                      "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                      + "\ncontentRecipient -- while waiting for PublishWithReo request"
                      + "\nreceived unexpected protocol message : " + ePublishWithReo
                      + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                    )
                  )
                }
              }
            }
          }
        }         
      }
      case _ => {
        BasicLogService.tweet(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\ncontentRecipient -- at least one cnxn expected : " + cnxns
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        println(
          (
            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
            + "\ncontentRecipient -- at least one cnxn expected : " + cnxns
            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
          )
        )
        throw new Exception( "at least one cnxn expected : " + cnxns )
      }
    }
  }
}

class ContentRecipientBehavior(
) extends ContentRecipientBehaviorT {
  override def run(
    kvdbNode: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    cnxns: Seq[PortableAgentCnxn],
    filters: Seq[CnxnCtxtLabel[String, String, String]]
  ): Unit = {
    super.run(kvdbNode, cnxns, filters)
  }
}

object ContentRecipientBehavior {
  def apply( ) : ContentRecipientBehavior = new ContentRecipientBehavior()
  def unapply( cb : ContentRecipientBehavior ) = Some( () )
}
