package com.ati.iaservices.helpers

import java.util.UUID
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.core.messages.Identification
import com.protegra_ati.agentservices.core.messages.content.{SetContentResponse, SetSelfContentRequest}
import com.protegra_ati.agentservices.core.schema.ConnectionCategory
import com.protegra_ati.agentservices.core.schema.disclosure.TrustLevel
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent

abstract class CreateConnectionHelper {
  def handleConnectionCompleted(connection: Connection)

  def request(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, aAlias: String, aTargetCnxn: AgentCnxnProxy, bAlias: String, bTargetCnxn: AgentCnxnProxy) {

    val aId = UUID.randomUUID().toString
    val bId = UUID.randomUUID().toString

    val connAB = ConnectionFactory.createConnection(bAlias, ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Trusted.toString, aId, bId)
    val connBA = ConnectionFactory.createConnection(aAlias, ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Trusted.toString, bId, aId)

    sendCreateConnectionRequest(ui, agentSessionId, tag, aTargetCnxn, connAB)
    sendCreateConnectionRequest(ui, agentSessionId, tag, bTargetCnxn, connBA)
  }

  private def sendCreateConnectionRequest(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, targetCnxn: AgentCnxnProxy, conn: Connection) {

    val eventKey = new EventKey(agentSessionId, tag)
    val req = new SetSelfContentRequest(new Identification(), eventKey, conn, null)
    req.targetCnxn = targetCnxn
    req.originCnxn = targetCnxn

    ui.send(req)
  }

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def setContentResponseReceived(e: SetContentResponseReceivedEvent) {
        val response: SetContentResponse = e.msg
        response.data match {
          case connection: Connection => {
            handleConnectionCompleted(connection)
          }
          case _ => {}
        }
      }
    })

  }
}
