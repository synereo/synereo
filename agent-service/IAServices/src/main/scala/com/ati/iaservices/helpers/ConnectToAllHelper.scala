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
import com.protegra_ati.agentservices.store.extensions.StringExtensions._


/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 10/04/13
 * Time: 10:35 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class ConnectToAllHelper {
  def handleConnectionsCompleted()

  def connectToAll(ui: AgentHostUIPlatformAgent, selfCnxn: AgentCnxnProxy, agentSessionId: UUID, selfAlias: String, userAgentId: UUID): Unit = {

    val APP_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
    val eventKey = "request_all_connections"

    def getAgentCnxnProxy(agentId:UUID): AgentCnxnProxy = {
      new AgentCnxnProxy(agentId.toString.toURI, "", agentId.toString.toURI )
    }

    def requestAllConnections(tag: String): Unit = {
      val req = new GetContentRequest(new EventKey(agentSessionId, tag), Connection.SEARCH_ALL)
      req.targetCnxn = getAgentCnxnProxy(APP_AGENT_ID)
      ui.send(req)
    }

    def listenRequestAllConnections(tag: String): Unit = {
      ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent): Unit = {
          val response : GetContentResponse = e.msg.asInstanceOf[GetContentResponse]

          val connections = response.data.toList.map(_.asInstanceOf[Connection]).toList

          for (connection <- connections) {
            // if the connection does not already involve the agent we are connecting, then create it
            // note that the ids will only match in the case of registration, where the agentId is used
            // as the id for the self cnxn.
            if (connection.alias != selfAlias) {
              val agentId = UUID.fromString(connection.writeCnxn.trgt.getHost())
              val agentCnxn = getAgentCnxnProxy(agentId)
              createConnection(selfAlias, selfCnxn, connection.getAlias(), agentCnxn)
            }
          }

          handleConnectionsCompleted()
        }
      })
    }

    def sendCreateConnectionRequest(targetCnxn: AgentCnxnProxy, conn: Connection) = {

      val eventKey = new EventKey(UUID.randomUUID, "")
      val req = new SetSelfContentRequest(new Identification(), eventKey, conn, null)
      req.targetCnxn = targetCnxn
      req.originCnxn = targetCnxn

      ui.send(req)
    }

    def createConnection (
          aAlias: String,
          aTargetCnxn: AgentCnxnProxy,
          bAlias: String,
          bTargetCnxn: AgentCnxnProxy): Unit = {

      val aId = UUID.randomUUID().toString
      val bId = UUID.randomUUID().toString

      val connAB = ConnectionFactory.createConnection(bAlias, ConnectionCategory.Person.toString(), ConnectionCategory.Person.toString(), TrustLevel.Trusted.toString(), aId, bId)
      val connBA = ConnectionFactory.createConnection(aAlias, ConnectionCategory.Person.toString(), ConnectionCategory.Person.toString(), TrustLevel.Trusted.toString(), bId, aId)

      sendCreateConnectionRequest(aTargetCnxn, connAB )
      sendCreateConnectionRequest(bTargetCnxn, connBA )

    }

    listenRequestAllConnections(eventKey)
    requestAllConnections(eventKey)
  }
}
