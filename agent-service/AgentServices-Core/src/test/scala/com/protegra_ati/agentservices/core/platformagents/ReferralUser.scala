package com.protegra_ati.agentservices.core.platformagents

import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.messages.invitation._
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events._
import java.util.UUID

trait ReferralUser
{
  /**
   * Simulates broker. Broker forwards the request, creating 2 referral requests to both parties, adds also his own posts to both parties
   * Broker reads/compares initial message from the source of the conversation to him
   * @param ui
   * @param connBrokerSelf
   * @param agentSessionId
   * @param tag
   * @param postToTarget  broker's post to target of the conversation
   * @param postToSource  broker's post to source of the conversation
   */
  def autoAcceptReferralRequest(ui: AgentHostUIPlatformAgent,
    connBrokerSelf: Connection,
    agentSessionId: UUID,
    tag: String,
    postToTarget: Post,
    postToSource: Post,
    handler: ( Post ) => Unit) =
  {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            x.data.foreach(datum => {
              datum match {
                case persistedMessage: PersistedMessage[ _ ] => {
                  val referralRequest = persistedMessage.message.asInstanceOf[ ReferralRequest ]
                  // handler has access to the inviterToBroker post
                  handler(referralRequest.source.postToBroker)
                  //responding message
                  val response = new ReferralResponse(referralRequest.ids.copyAsChild(), referralRequest.eventKey, referralRequest.source, postToTarget, postToSource, true)
                  response.targetCnxn = referralRequest.targetCnxn
                  response.originCnxn = referralRequest.originCnxn

                  //TODO: jsk - temporary - too close in unit test, causing db concurrency issues?
                  Thread.sleep(500)
                  ui.send(response)
                }
                case _ => {}
              }
            })

          }
          case _ => {}
        }
      }
    });

    val getReq = new GetContentRequest(new EventKey(agentSessionId, tag), ReferralRequest.SEARCH_ALL_PERSISTED_MESSAGE)
    getReq.targetCnxn = connBrokerSelf.writeCnxn
    ui.send(getReq)
  }

  /**
   * Simulates broker. Broker forwards the request, creating 2 referral requests to both parties, adds also his own posts to both parties
   * Broker reads/compares initial message from the source of the conversation to him
   * @param ui
   * @param connBrokerSelf
   * @param agentSessionId
   * @param tag
   * @param postToSource  broker's post to source of the conversation
   */
  def autoRejectReferralRequest(ui: AgentHostUIPlatformAgent,
    connBrokerSelf: Connection,
    agentSessionId: UUID,
    tag: String,
    postToSource: Post,
    handler: ( Post ) => Unit) =
  {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            x.data.foreach(datum => {
              datum match {
                case persistedMessage: PersistedMessage[ _ ] => {
                  val referralRequest = persistedMessage.message.asInstanceOf[ ReferralRequest ]
                  // handler has access to the inviterToBroker post
                  handler(referralRequest.source.postToBroker)
                  //responding message
                  // The target post doesn't matter if  ReferralResponse is not accepted
                  val response = new ReferralResponse(referralRequest.ids.copyAsChild(), referralRequest.eventKey, referralRequest.source, new Post (), postToSource, false)
                  response.targetCnxn = referralRequest.targetCnxn
                  response.originCnxn = referralRequest.originCnxn
                  //TODO: jsk - temporary - too close in unit test, causing db concurrency issues?
                  Thread.sleep(500)
                  ui.send(response)
                }
                case _ => {}
              }
            })

          }
          case _ => {}
        }
      }
    });

    val getReq = new GetContentRequest(new EventKey(agentSessionId, tag), ReferralRequest.SEARCH_ALL_PERSISTED_MESSAGE)
    getReq.targetCnxn = connBrokerSelf.writeCnxn
    ui.send(getReq)
  }

}
