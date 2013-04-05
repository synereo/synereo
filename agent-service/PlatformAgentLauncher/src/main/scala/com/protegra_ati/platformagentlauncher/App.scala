package com.protegra_ati.platformagentlauncher

import java.io.File
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.core.messages.admin.{RegistrationResponse, RegistrationRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent, AgentHostUIPlatformAgent}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

//run me with "mvn scala:run"
object App
{
  final val STORE_CONFIG = "init_store.conf"
  final val UI_CONFIG = "init_ui.conf"
  final val DB_STORE_CONFIG = "db_store.conf"
  final val DB_STORE_DB_CONFIG = "db_store_db.conf"
  final val DB_STORE_PUBLIC_CONFIG = "db_store_public.conf"
  final val LOG_KVDB_CONFIG = "log.conf"
  final val LOG_KVDB_PROPERTIES = "log.properties"
  final val LOG_AGENTSERVICES_CONFIG = "log_agentservices.conf"
  final val LOG_AGENTSERVICES_PROPERTIES = "log_agentservices.properties"

  def main(args: Array[ String ]): Unit =
  {
    var ui : AgentHostUIPlatformAgent = null
    var store : AgentHostStorePlatformAgent = null

    if (args.length > 0) {
      if (args.contains("store")) {
        store = new AgentHostStorePlatformAgent()
        checkAllStoreConfigFiles()
        store.initFromConfig(STORE_CONFIG)
        println("*************** StorePlatformAgent launcher started ***************")
      }

      if (args.contains("ui")) {
        ui = new AgentHostUIPlatformAgent()
        configFileExists(UI_CONFIG)
        ui.initFromConfig(UI_CONFIG)
        println("*************** UIPlatformAgent launcher started ***************")
      }

      // ACTIONS
      if (args.contains("registerAgent")) {
        if (ui != null) {
          registerAgent(ui)
        }
      }
    }
  }

  def registerAgent(ui : AgentHostUIPlatformAgent) = {
    println("*************** Start RegisterAgent ***************")

    val selfAlias = "John Smith"
    val agentSessionId = UUID.randomUUID
    val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
    val eventKey = "registration"

    val userAgentId = UUID.randomUUID
    val selfCnxn = new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )

    def requestRegistration(ui: AgentHostUIPlatformAgent, selfAlias: String, agentSessionId: UUID, tag: String) = {
      val req = new RegistrationRequest(new EventKey(agentSessionId, tag), BIZNETWORK_AGENT_ID, selfAlias)
      req.targetCnxn = selfCnxn
      ui.send(req)
    }

    def listenRegistrationResponse(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) = {
      ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
        {
          val response : RegistrationResponse = e.msg.asInstanceOf[RegistrationResponse]
          val newAgentId = response.agentId
          println("*************** RegistrationResponse ---------------")
          println(e.toString)
          println("--------------- RegistrationResponse ***************")
          println("*************** New AgentId = " + newAgentId + "***************")
          println("*************** Finish RegisterAgent ***************")
        }
      });
    }

    requestRegistration(ui, selfAlias, agentSessionId, eventKey)
    listenRegistrationResponse(ui, agentSessionId, eventKey);
  }

  //refactor this into core
  def checkAllStoreConfigFiles() = {
    configFileExists(STORE_CONFIG)
    configFileExists(DB_STORE_CONFIG)
    configFileExists(DB_STORE_DB_CONFIG)
    configFileExists(DB_STORE_PUBLIC_CONFIG)
    configFileExists(LOG_KVDB_CONFIG)
    configFileExists(LOG_KVDB_PROPERTIES)
    configFileExists(LOG_AGENTSERVICES_CONFIG)
    configFileExists(LOG_AGENTSERVICES_PROPERTIES)
  }

  //refactor this into core and a util object
  def configFileExists(filePath: String) =
  {
    val f = new File(filePath)
    if ( !f.exists() )
    {
      val errorMessage = "Missing config file for : " + filePath
      println(errorMessage)
      throw new Exception(errorMessage)
    }
  }
}
