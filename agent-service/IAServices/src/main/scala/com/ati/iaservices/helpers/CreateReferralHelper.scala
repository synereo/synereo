package com.ati.iaservices.helpers

import com.ati.iaservices.events.{MessageFactory,MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{Post, AgentCnxnProxy, Data}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import com.ati.iaservices.events.CreateReferralResponseReceivedEvent
import java.util.UUID

abstract class CreateReferralHelper {
  def handleListen(e: CreateReferralResponseReceivedEvent)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, target: AgentCnxnProxy)
             (invitationConnectionId_A: String, alias_A: String, post_A: Post)
             (invitationConnectionId_B: String, alias_B: String, post_B: Post)
 {
    val req = MessageFactory.createCreateReferralRequest( agentSessionId, tag, target )( invitationConnectionId_A,alias_A, post_A )( invitationConnectionId_B, alias_B, post_B )
    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def createReferralResponseReceived(e: CreateReferralResponseReceivedEvent) {
        handleListen(e)
      }
    })
  }
}
