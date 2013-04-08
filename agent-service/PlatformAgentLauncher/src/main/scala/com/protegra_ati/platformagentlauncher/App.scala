package com.protegra_ati.platformagentlauncher

import java.io.File
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.{Profile, AgentCnxnProxy}
import com.protegra_ati.agentservices.core.messages.admin.{RegistrationResponse, RegistrationRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, RegistrationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent, AgentHostUIPlatformAgent}
import com.protegra_ati.agentservices.core.messages.content.{SetContentResponse, SetContentRequest}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.ati.iaservices.schema.Label
import scala.util.Random

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

  final val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")


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
          val selfAlias = "John Smith"
          val agentSessionId = UUID.randomUUID
          registerAgent(ui, agentSessionId, selfAlias)
        }
      }
    }
  }

  def registerAgent(ui : AgentHostUIPlatformAgent, agentSessionId : UUID, selfAlias : String) = {
    println("*************** Start RegisterAgent ***************")
    val eventKey = "registration"

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

          val profile = createRandomProfile()
          saveProfile(ui, agentSessionId, newAgentId, profile)
        }
      })
    }

    def requestRegistration(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String, selfAlias: String) = {
      val req = new RegistrationRequest(new EventKey(agentSessionId, tag), BIZNETWORK_AGENT_ID, selfAlias)
      ui.send(req)
    }

    listenRegistrationResponse(ui, agentSessionId, eventKey)
    requestRegistration(ui, agentSessionId, eventKey, selfAlias)
  }

  def saveProfile(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, agentId : UUID, profile : Profile) = {
    println("*************** Start SaveProfile ***************")
    val eventKey = "profile"

    def listenSaveProfileResponse(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) = {
      ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          val response : SetContentResponse = e.msg.asInstanceOf[SetContentResponse]
          println("*************** SetContentResponse ---------------")
          println(e.toString)
          println("--------------- SetContentResponse ***************")
          println("*************** Finish SaveProfile ***************")
        }
      })
    }

    def requestSaveProfile(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, agentId: UUID, tag: String, profile: Profile) = {
      val req = new SetContentRequest(new EventKey(agentSessionId, tag), profile, null)
      req.setTargetCnxn(new AgentCnxnProxy(agentId.toString.toURI, "", agentId.toString.toURI))
      ui.send(req)
    }

    listenSaveProfileResponse(ui, agentSessionId, eventKey)
    requestSaveProfile(ui, agentSessionId, agentId, eventKey, profile)
  }


  def createRandomProfile() = {
    val profile = new Profile()
    profile.setFirstName(createRandomWord(5))
    profile.setLastName(createRandomWord(8))
    profile.setCity("Winnipeg")
    profile.setRegion("MB")
    profile.setCountry("Canada")
    profile.setEmailAddress(createRandomWord(6) + "@" + createRandomWord(6) + ".com")
    profile
  }

  def createRandomWord(length: Int):String = {
    def safeChar() = {
      val res = (Random.nextInt('z' - 'a') + 'a').toChar
      res.toChar
    }

    List.fill(length)(safeChar()).mkString
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
