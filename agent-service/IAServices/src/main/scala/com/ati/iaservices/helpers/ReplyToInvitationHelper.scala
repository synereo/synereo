package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.events.{CreateInvitationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{Post, AgentCnxnProxy}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import java.util.UUID
import com.protegra_ati.agentservices.core.messages.invitation.{CreateInvitationResponse, InvitationRequest}

abstract class ReplyToInvitationHelper {
  def handleListen(msg: CreateInvitationResponse)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, target: AgentCnxnProxy)
             (category: String, connectionType: String, connectionName: String, post: Post, conversationThread: java.util.List[ Post ], accept: Boolean, roleBasedAlias: String, invitationRequest: InvitationRequest )
  {
    val req = MessageFactory.createInvitationResponse(agentSessionId, tag, target)(category, connectionType, connectionName, post, conversationThread, accept, roleBasedAlias, invitationRequest)
    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def createInvitationResponseReceived(e: CreateInvitationResponseReceivedEvent) {
        handleListen(e.getMsg)
      }
    })
  }
}
