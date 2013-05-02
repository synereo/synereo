package com.ati.iaservices.helpers

import com.ati.iaservices.events.MessageFactory
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import java.util.UUID
import scala.collection.JavaConversions._

abstract class GetContentHelper[T <: Data] {
  def handleListen(data: T) {}

  def handleListen(data: List[Data]) {}

  def request(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String, queryObject: Data, target: AgentCnxnProxy) {
    val req = MessageFactory.createGetContentRequest(agentSessionId, tag, queryObject, target)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
        handleListen(e.getMsg.data.toList)
        for (datum <- e.getMsg.data) {
          handleListen(datum.asInstanceOf[T])
        }
      }
    })
  }
}
