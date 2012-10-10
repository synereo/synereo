/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.events._
import java.util.UUID
import com.protegra_ati.agentservices.core.messages._
import admin.{RegistrationResponse, RegistrationRequest}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.util.{Severity, Reporting}
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import content.{SetContentAdminRequest, SetContentAdminResponse, SetContentResponse, SetContentRequest}
import scala.collection.JavaConversions._

//import com.sun.org.apache.xpath.internal.operations._

import com.protegra_ati.agentservices.core.util._


trait Registration extends Reporting
{
  self: AgentHostUIPlatformAgent =>

  val TEST_APP_ID = UUID.fromString("aaaa533a-d417-4d71-ad94-8c766907381b")
  var TEST_AGENT_ID = UUID.fromString("ffff533a-d417-4d71-ad94-8c766907381b")

  val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
  val BIZNETWORK_APP_NAME = "BizNetwork App"

    def register(msg: RegistrationRequest): Unit =
  {
    //no matter what appId is given, hardcoded to BizNetwork for now
    report("in register for app: " + msg.appId.toString)
    //lookup from appId -> appAgentId
    val appAgentId = getAppAgentId(msg.appId)
    val connAppSelf = ConnectionFactory.createConnection(BIZNETWORK_APP_NAME, ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "admin", appAgentId.toString, appAgentId.toString)

    val agentId = UUID.randomUUID()
    val connAgentSelf = ConnectionFactory.createConnection(msg.agentName, ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "admin", agentId.toString, agentId.toString)

    listenForSetContentAdminResponse(
      msg.eventKey,
      saveData(_: SetContentAdminResponse, connAppSelf, connAgentSelf, agentId, appAgentId)
    )

    sendSetContentAdminRequest(msg.eventKey, connAppSelf, connAgentSelf)
  }

  def getAppAgentId(appId: UUID): UUID = {
    if (appId == TEST_APP_ID)
      TEST_AGENT_ID
    else
      BIZNETWORK_AGENT_ID
  }

  def listenForSetContentAdminResponse(
    eventKey: EventKey,
    handler: ( SetContentAdminResponse ) => Unit
    ) =
  {
    addListener(eventKey.agentSessionId, "CONN_SAVED_TO_STORE", new MessageEventAdapter(eventKey.eventTag)
    {
      override def setContentAdminResponseReceived(e: SetContentAdminResponseReceivedEvent) =
      {
        handler(e.msg)
      }
    })
  }

  def sendSetContentAdminRequest(eventKey: EventKey, connAppSelf: Connection, newData: Data) =
  {
    val setReq = new SetContentAdminRequest(eventKey, newData, null)
    setReq.originCnxn = connAppSelf.writeCnxn
    setReq.targetCnxn = connAppSelf.writeCnxn
    report("ABOUT TO SAVE BOOTSTRAPPED CONNECTION: ", Severity.Info)
    send(setReq)
  }

  def saveData(msg: SetContentAdminResponse, connAppSelf: Connection, connAgentSelf: Connection, agentId: UUID, appAgentId: UUID) =
  {
    //TODO: make this transactional?
    saveAppId(msg, connAppSelf, connAgentSelf, agentId, appAgentId)
    saveConnections(msg, connAppSelf, connAgentSelf, agentId, appAgentId)
  }
  def saveAppId(msg: SetContentAdminResponse, connAppSelf: Connection, connAgentSelf: Connection, agentId: UUID, appAgentId: UUID) =
  {
    val appId = AppId(BIZNETWORK_APP_NAME)
    sendSetContentRequest(msg.eventKey.copy(agentSessionId = UUID.randomUUID()), connAgentSelf, appId)
  }

  def saveConnections(msg: SetContentAdminResponse, connAppSelf: Connection, connAgentSelf: Connection, agentId: UUID, appAgentId: UUID) =
  {
    val connAgentApp = ConnectionFactory.createConnection(BIZNETWORK_APP_NAME, ConnectionCategory.Person.toString, ConnectionCategory.App.toString, "admin", agentId.toString, appAgentId.toString)
    val connAppAgent = ConnectionFactory.createConnection(connAgentSelf.alias, ConnectionCategory.App.toString, ConnectionCategory.Person.toString, "admin", appAgentId.toString, agentId.toString)
    // Triggering set of conn.id
    val tempKey = connAppAgent.toStoreKey

    //we only want to trigger the listen of our self setup
    sendSetContentRequest(msg.eventKey.copy(agentSessionId = UUID.randomUUID()), connAppSelf, connAppAgent)

    //ok to use the eventKey as the UI won't listen to SetContentResponse on that eventTag, it will listen for RegistrationResponse
    listenForSetContentResponse(msg.eventKey, sendRegistrationResponse(_: SetContentResponse, agentId, connAppAgent.id))
    sendSetContentRequest(msg.eventKey, connAgentSelf, connAgentApp)
  }

  def sendSetContentRequest(eventKey: EventKey, connSelf: Connection, newData: Data) =
  {
    val setReq = new SetContentRequest(eventKey, newData, null)
    setReq.originCnxn = connSelf.writeCnxn
    setReq.targetCnxn = connSelf.writeCnxn
    report("ABOUT TO SAVE BOOTSTRAPPED CONNECTION: ", Severity.Info)
    send(setReq)
  }

  def listenForSetContentResponse(
    eventKey: EventKey,
    handler: ( SetContentResponse ) => Unit
    )
  {
    this.addListener(eventKey.agentSessionId, "CONN_SAVED_TO_SELF", new MessageEventAdapter(eventKey.eventTag)
    {
      override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
      {
        handler(e.msg)
      }
    })
  }

  def sendRegistrationResponse(msg: SetContentResponse, agentId: UUID, brokerToAgentId: String) =
  {
    val connSelf = ConnectionFactory.createSelfConnection("self", agentId.toString)
    val response = new RegistrationResponse(msg.ids.copyAsChild(), msg.eventKey.copy(), agentId, connSelf, brokerToAgentId)
    triggerEvent(response.generateEvent())
  }

}
