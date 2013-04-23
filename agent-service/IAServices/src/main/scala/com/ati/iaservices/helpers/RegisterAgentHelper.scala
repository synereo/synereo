package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import java.util.UUID

abstract class RegisterAgentHelper {
  def handleListen(response: RegistrationResponse)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, appId: UUID, alias: String) {
    val req = MessageFactory.createRegistrationRequest(agentSessionId, tag, appId, alias)
    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) {
        handleListen(e.msg)
      }
    })
  }
}
