package com.ati.iaservices.helpers

import java.util.UUID
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.core.messages.Identification
import com.protegra_ati.agentservices.core.messages.content.{GetContentResponse, GetContentRequest, SetSelfContentRequest}
import com.protegra_ati.agentservices.core.schema.ConnectionCategory
import com.protegra_ati.agentservices.core.schema.disclosure.TrustLevel
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent

abstract class ConnectToAllHelper {
  def handleConnectionsCompleted()

  def connectToAll(ui: AgentHostUIPlatformAgent, selfCnxn: AgentCnxnProxy, agentSessionId: UUID, selfAlias: String) {

    val APP_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
    val selfAgentId = UUID.fromString(selfCnxn.trgt.getHost)
    val eventKey = "request_all_connections"

    def requestAllConnections(tag: String) {
      val req = new GetContentRequest(new EventKey(agentSessionId, tag), Connection.SEARCH_ALL)
      req.targetCnxn = ConnectionFactory.createSelfConnection("", APP_AGENT_ID.toString).writeCnxn
      ui.send(req)
    }

    def listenRequestAllConnections(tag: String) {
      ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
          val response: GetContentResponse = e.msg

          val connections = response.data.toList.map(_.asInstanceOf[Connection]).toList

          for (connection <- connections) {
            // if the connection does not already involve the agent we are connecting, then create it
            val connectionAgentId = UUID.fromString(connection.writeCnxn.trgt.getHost)
            if (connectionAgentId != selfAgentId) {
              createConnection(selfAlias, selfCnxn, connection.getAlias, ConnectionFactory.createSelfConnection("", connectionAgentId.toString).writeCnxn)
            }
          }

          handleConnectionsCompleted()
        }
      })
    }

    def sendCreateConnectionRequest(targetCnxn: AgentCnxnProxy, conn: Connection) {

      val eventKey = new EventKey(UUID.randomUUID, "")
      val req = new SetSelfContentRequest(new Identification(), eventKey, conn, null)
      req.targetCnxn = targetCnxn
      req.originCnxn = targetCnxn

      ui.send(req)
    }

    def createConnection(aAlias: String, aTargetCnxn: AgentCnxnProxy, bAlias: String, bTargetCnxn: AgentCnxnProxy) {

      val aId = UUID.randomUUID().toString
      val bId = UUID.randomUUID().toString

      val connAB = ConnectionFactory.createConnection(bAlias, ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Trusted.toString, aId, bId)
      val connBA = ConnectionFactory.createConnection(aAlias, ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Trusted.toString, bId, aId)

      sendCreateConnectionRequest(aTargetCnxn, connAB)
      sendCreateConnectionRequest(bTargetCnxn, connBA)
    }

    listenRequestAllConnections(eventKey)
    requestAllConnections(eventKey)
  }
}
