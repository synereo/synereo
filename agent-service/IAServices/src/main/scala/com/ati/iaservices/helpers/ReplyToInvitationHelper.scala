package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.events.{CreateInvitationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{Post, AgentCnxnProxy}
import java.util.UUID
import com.protegra_ati.agentservices.core.messages.invitation.{CreateInvitationResponse, InvitationRequest}
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent

abstract class ReplyToInvitationHelper {
  def handleListen(msg: CreateInvitationResponse)

  // scalastyle:off parameter.number
  def request(dsl: AgentHostDslPlatformAgent,
              agentSessionId: UUID,
              tag: String,
              target: AgentCnxnProxy)
             (category: String,
              connectionType: String,
              connectionName: String,
              post: Post,
              conversationThread: java.util.List[Post],
              accept: Boolean,
              roleBasedAlias: String,
              invitationRequest: InvitationRequest)
  // scalastyle:on parameter.number
  {
    val req = MessageFactory.createInvitationResponse(agentSessionId,
                                                      tag,
                                                      target)(category,
                                                      connectionType,
                                                      connectionName,
                                                      post,
                                                      conversationThread,
                                                      accept,
                                                      roleBasedAlias,
                                                      invitationRequest)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def createInvitationResponseReceived(e: CreateInvitationResponseReceivedEvent) {
        handleListen(e.getMsg)
      }
    })
  }
}
