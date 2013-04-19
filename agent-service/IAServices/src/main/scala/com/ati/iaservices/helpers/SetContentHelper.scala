package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import java.util.UUID

abstract class SetContentHelper[T <: Data] {
  def handleListen(data: T)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, data: Data, target: AgentCnxnProxy) {
    val req = MessageFactory.createSetContentRequest(agentSessionId, tag, data, target)
    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def setContentResponseReceived(e: SetContentResponseReceivedEvent) {
        handleListen(e.getMsg.data.asInstanceOf[T])
      }
    })
  }
}
