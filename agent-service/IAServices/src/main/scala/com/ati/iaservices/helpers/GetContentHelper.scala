package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import java.util.UUID
import scala.collection.JavaConversions._

abstract class GetContentHelper[T <: Data] {
  def handleListen(data: T)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, queryObject: Data, target: AgentCnxnProxy) {
    val req = MessageFactory.createGetContentRequest(agentSessionId, tag, queryObject, target)
    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
        for (datum <- e.getMsg.data) {
          handleListen(datum.asInstanceOf[T])
        }
      }
    })
  }
}
