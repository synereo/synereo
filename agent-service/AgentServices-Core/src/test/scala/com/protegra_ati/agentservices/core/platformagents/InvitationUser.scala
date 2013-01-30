package com.protegra_ati.agentservices.core.platformagents

import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.messages.invitation._
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events._
import java.util.UUID

/* User: mgevantmakher
*/

trait InvitationUser
{

  def autoAcceptInvitationRequest(ui: AgentHostUIPlatformAgent, connInviteeSelf: Connection, category: String, connectionType: String, connectionName: String, agentSessionId: UUID, tag: String, assertionHandler: (AgentHostUIPlatformAgent, Connection, String, String, UUID, String, InvitationRequest, InvitationResponse) => Unit) =
  {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            x.data.foreach(datum => {
              datum match {
                case persistedMessage: PersistedMessage[ InvitationRequest ] => {
                  val inviteRequest = persistedMessage.message.asInstanceOf[ InvitationRequest ]
                  val response = new InvitationResponse(inviteRequest.ids.copyAsChild(), inviteRequest.eventKey, category, connectionType, connectionName, null, inviteRequest.conversationThread, true, "", inviteRequest)
//                  response.targetCnxn = inviteRequest.targetCnxn
//                  response.originCnxn = inviteRequest.originCnxn
                  // can throw assertion error
                  assertionHandler(ui, connInviteeSelf, connectionType, connectionName, agentSessionId, tag, inviteRequest, response)
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

    val query: PersistedMessage[ InvitationRequest ] = new PersistedMessage[ InvitationRequest ]()
    val getReq = new GetContentRequest(new EventKey(agentSessionId, tag), query)
    getReq.targetCnxn = connInviteeSelf.writeCnxn
    ui.send(getReq)
  }

  // duplication becaues of two different handler one for check of posts, another to check requests
  def autoAcceptInvitationRequest(ui: AgentHostUIPlatformAgent, connInviteeSelf: Connection, category:String, connectionType: String, connectionName: String, agentSessionId: UUID, tag: String, handler: ( List[ Post ] ) => Unit) =
  {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            x.data.foreach(datum => {
              datum match {
                case persistedMessage: PersistedMessage[ InvitationRequest ] => {
                  val inviteRequest = persistedMessage.message.asInstanceOf[ InvitationRequest ]
                  handler(inviteRequest.conversationThread.toList)
                  val response = new InvitationResponse(inviteRequest.ids.copyAsChild(), inviteRequest.eventKey, category, connectionType, connectionName, null, inviteRequest.conversationThread, true, "", inviteRequest)
//                  response.targetCnxn = inviteRequest.targetCnxn
//                  response.originCnxn = inviteRequest.originCnxn

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

    val query: PersistedMessage[ InvitationRequest ] = new PersistedMessage[ InvitationRequest ]()
    val getReq = new GetContentRequest(new EventKey(agentSessionId, tag), query)
    getReq.targetCnxn = connInviteeSelf.writeCnxn
    ui.send(getReq)
  }

  def autoRejectInvitationRequest(ui: AgentHostUIPlatformAgent, connInviteeSelf: Connection, category:String, connectionType: String, connectionName: String, agentSessionId: UUID, tag: String, handler: ( List[ Post ] ) => Unit, responseMessageText: String) =
  {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            x.data.foreach(datum => {
              datum match {
                case persistedMessage: PersistedMessage[ InvitationRequest ] => {
                  val inviteRequest = persistedMessage.message.asInstanceOf[ InvitationRequest ]
                  handler(inviteRequest.conversationThread.toList)
                  val response = new InvitationResponse(inviteRequest.ids.copyAsChild(),
                    inviteRequest.eventKey,
                    category,
                    connectionType,
                    connectionName,
                    new Post("I'm rejecting", "theBody", new java.util.HashMap(), new java.util.HashMap()), inviteRequest.conversationThread, false, "", inviteRequest)
//                  response.targetCnxn = inviteRequest.targetCnxn
//                  response.originCnxn = inviteRequest.originCnxn
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

    val query: PersistedMessage[ InvitationRequest ] = new PersistedMessage[ InvitationRequest ]()
    val getReq = new GetContentRequest(new EventKey(agentSessionId, tag), query)
    getReq.targetCnxn = connInviteeSelf.writeCnxn
    ui.send(getReq)
  }

}
