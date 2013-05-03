package com.ati.iaservices.helpers

import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}
import java.util.UUID

abstract class RegisterAgentHelper {
  def handleListen(response: RegistrationResponse)

  def request(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String, appId: UUID, alias: String) {
    val req = MessageFactory.createRegistrationRequest(agentSessionId, tag, appId, alias)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) {
        handleListen(e.msg)
      }
    })
  }
}
