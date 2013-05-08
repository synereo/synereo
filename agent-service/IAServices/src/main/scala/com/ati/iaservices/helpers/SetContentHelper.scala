package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import java.util.UUID

abstract class SetContentHelper[T <: Data] {
  def handleListen(data: T)

  def request(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String, data: Data, target: AgentCnxnProxy) {
    val req = MessageFactory.createSetContentRequest(agentSessionId, tag, data, target)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def setContentResponseReceived(e: SetContentResponseReceivedEvent) {
        handleListen(e.getMsg.data.asInstanceOf[T])
      }
    })
  }
}
