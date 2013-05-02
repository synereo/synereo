package com.ati.iaservices.helpers

import com.ati.iaservices.events.{MessageFactory,MessageEventAdapter}
import com.ati.iaservices.messages.referral.Referral
import com.protegra_ati.agentservices.core.schema.{Post, AgentCnxnProxy}
import com.ati.iaservices.events.CreateReferralResponseReceivedEvent
import java.util.UUID
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent

abstract class CreateReferralHelper {
  def handleListen(e: CreateReferralResponseReceivedEvent)

  def request(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String, target: AgentCnxnProxy)(referral_A: Referral)(referral_B: Referral)
 {
    val req = MessageFactory.createCreateReferralRequest(agentSessionId, tag, target)(referral_A)(referral_B)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def createReferralResponseReceived(e: CreateReferralResponseReceivedEvent) {
        handleListen(e)
      }
    })
  }
}
