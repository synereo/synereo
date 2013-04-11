package com.ati.iaservices.events

import com.protegra_ati.agentservices.core.messages.content.{SetContentRequest, GetContentRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.schema.{Data, AgentCnxnProxy}
import java.util.UUID

object MessageFactory {
  def createGetContentRequest(agentSessionId: UUID, tag: String, queryObject: Data, target: AgentCnxnProxy): GetContentRequest = {
    val eventKey: EventKey = new EventKey(agentSessionId, tag)
    val msg: GetContentRequest = new GetContentRequest(eventKey, queryObject)
    msg.setTargetCnxn(target)
    return msg
  }

  def createSetContentRequest(agentSessionId: UUID, tag: String, newData: Data, target: AgentCnxnProxy): SetContentRequest = {
    val eventKey: EventKey = new EventKey(agentSessionId, tag)
    val msg: SetContentRequest = new SetContentRequest(eventKey, newData, null)
    msg.setTargetCnxn(target)
    return msg
  }
}
