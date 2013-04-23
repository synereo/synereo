package com.ati.iaservices.helpers

import java.util.UUID
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.core.messages.content.{GetContentResponse, GetContentRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent

abstract class ConnectToAllHelper {
  def handleConnectionsCompleted()

  def connectToAll(ui: AgentHostUIPlatformAgent, selfCnxn: AgentCnxnProxy, agentSessionId: UUID, selfAlias: String, sourceAgentId: UUID) {
    val eventKey = "request_all_connections"
    listenRequestAllConnections(ui, agentSessionId, eventKey, selfCnxn, selfAlias, sourceAgentId)
    requestAllConnections(ui, agentSessionId, eventKey, sourceAgentId)
  }

  def requestAllConnections(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, sourceAgentId: UUID) {
    val req = new GetContentRequest(new EventKey(agentSessionId, tag), Connection.SEARCH_ALL)
    req.targetCnxn = ConnectionFactory.createSelfConnection("", sourceAgentId.toString).writeCnxn
    ui.send(req)
  }

  def listenRequestAllConnections(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, selfCnxn: AgentCnxnProxy, selfAlias: String, sourceAgentId: UUID) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
        val response: GetContentResponse = e.msg
        var expectedConnections = ((response.data.size - 1) * 2) // All of the connections, minus the self connection * 2 because connections are created in both directions
        response.data.foreach(datum => {
          datum match {
            case connection: Connection => {
              // if the connection does not already involve the agent we are connecting, then create it
              val connectionAgentId = UUID.fromString(connection.writeCnxn.trgt.getHost)
              val selfAgentId = UUID.fromString(selfCnxn.trgt.getHost)
              if (connectionAgentId != selfAgentId && connectionAgentId != sourceAgentId) {
                val createConnectionHelper = new CreateConnectionHelper {
                  def handleConnectionCompleted(connection: Connection) {
                    expectedConnections = expectedConnections - 1
                    if (expectedConnections == 0) {
                      handleConnectionsCompleted()
                    }
                  }
                }

                val tag = "connection" + UUID.randomUUID
                createConnectionHelper.listen(ui, agentSessionId, tag)
                createConnectionHelper.request(ui, agentSessionId, tag, selfAlias, selfCnxn,
                  connection.getAlias, ConnectionFactory.createSelfConnection("", connectionAgentId.toString).writeCnxn)
              }
            }
            case _ => {}
          }
        })
      }
    })
  }
}
