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
  def provideHashedConnections() : Seq[PortableAgentCnxn] 
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

                  val agntP2CPC = acT.AgentCnxn( p2cPC.src, p2cPC.label, p2cPC.trgt )

                  val calulateReoRequest = CalculateReo( sidPC, cidPC, p2cPC )
                  node.publish( agntP2CPC )( 
                    CalculateReo.toLabel( sidPC ), 
                    calulateReoRequest
                  )
                  for( eHashedConnections <- node.subscribe( agntCnxn )( HashedConnections.toLabel ) ) {
                    rsrc2V[ReputationMessage]( eHashedConnections ) match
                    {
                      case Left( HashedConnections( sidPC, cidPC, seqOfCnxnsPC ) ) => {
                        BasicLogService.tweet(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\ncontentRecipient -- received HashedConnections request: " + eHashedConnections
                            + "\ncnxn: " + agntCnxn
                            + "\nlabel: " + HashedConnections.toLabel
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        println(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\ncontentRecipient -- received HashedConnections request: " + eHashedConnections
                            + "\ncnxn: " + agntCnxn
                            + "\nlabel: " + HashedConnections.toLabel
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        val hashedConnections : Seq[PortableAgentCnxn] = provideHashedConnections()
                        val hashedConnectionsData = HashedConnections( sidPC, cidPC, hashedConnections )
                        node.publish( agntP2CPC )( 
                          HashedConnections.toLabel( sidPC ), 
                          hashedConnectionsData
                        )

                        val intersectionConnections : Seq[PortableAgentCnxn] = hashedConnections intersect seqOfCnxnsPC;
                        val intersectionConnectionsData = IntersectionResult( sidPC, cidPC, intersectionConnections);
                        for( eIntersectionResult <- node.subscribe( agntCnxn )( IntersectionResult.toLabel ) ) {
                          rsrc2V[ReputationMessage]( eIntersectionResult ) match
                          {
                            case Left( IntersectionResult( sidPC, cidPC, intersectCnxnsPC ) ) => {
                              BasicLogService.tweet(
                                (
                                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  + "\ncontentRecipient -- received IntersectionResult request: " + eIntersectionResult
                                  + "\ncnxn: " + agntCnxn
                                  + "\nlabel: " + IntersectionResult.toLabel
                                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                )
                              )
                              println(
                                (
                                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  + "\ncontentRecipient -- received IntersectionResult request: " + eIntersectionResult
                                  + "\ncnxn: " + agntCnxn
                                  + "\nlabel: " + IntersectionResult.toLabel
                                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                )
                              )
                              if ( intersectionConnections != intersectCnxnsPC ) {
                                BasicLogService.tweet(
                                  (
                                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                    + "\ncontentRecipient -- received IntersectionResult request: " + eIntersectionResult
                                    + "\ncnxn: " + agntCnxn
                                    + "\nlabel: " + IntersectionResult.toLabel
                                    + "\nInvalid connection intersection"
                                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  )
                                )
                                println(
                                  (
                                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                    + "\ncontentRecipient -- received IntersectionResult request: " + eIntersectionResult
                                    + "\ncnxn: " + agntCnxn
                                    + "\nlabel: " + IntersectionResult.toLabel
                                    + "\nInvalid connection intersection"
                                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  )
                                )
                              }
                              node.publish( agntP2CPC )( 
                                IntersectionResult.toLabel( sidPC ), 
                                intersectionConnectionsData
                              )
                              val reoScore : Int = 1000; // placeholder for actual calculation
                              if ( reoScore != reoPC ) {
                                BasicLogService.tweet(
                                  (
                                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                    + "\nInvalid reo!!"
                                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  )
                                )
                                println(
                                  (
                                    "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                    + "\nInvalid reo!!"
                                    + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  )
                                )
                              }
                              // reo is good, continue to publish
                            }
                            case Right( true ) => {
                              BasicLogService.tweet(
                                (
                                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  + "\ncontentRecipient -- still waiting for IntersectionResult request"
                                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                )
                              )
                              println(
                                (
                                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  + "\ncontentRecipient -- still waiting for IntersectionResult request"
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
                                  + "\ncontentRecipient -- while waiting for IntersectionResult request"
                                  + "\nreceived unexpected protocol message : " + eIntersectionResult
                                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                )
                              )
                              println(
                                (
                                  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                                  + "\ncontentRecipient -- while waiting for IntersectionResult request"
                                  + "\nreceived unexpected protocol message : " + eIntersectionResult
                                  + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
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
                            + "\ncontentRecipient -- still waiting for HashedConnections request"
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        println(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\ncontentRecipient -- still waiting for HashedConnections request"
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
                            + "\ncontentRecipient -- while waiting for HashedConnections request"
                            + "\nreceived unexpected protocol message : " + eHashedConnections
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                          )
                        )
                        println(
                          (
                            "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
                            + "\ncontentRecipient -- while waiting for HashedConnections request"
                            + "\nreceived unexpected protocol message : " + eHashedConnections
                            + "\n||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
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
  def provideHashedConnections() : Seq[PortableAgentCnxn] =
    List[PortableAgentCnxn]( )
}

object ContentRecipientBehavior {
  def apply( ) : ContentRecipientBehavior = new ContentRecipientBehavior()
  def unapply( cb : ContentRecipientBehavior ) = Some( () )
}
