package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.ati.iaservices.helpers.RegisterAgentHelper
import com.protegra_ati.agentservices.store.util.LogConfiguration._

abstract class RegisterAgentPlugin extends LauncherPluginBase {
  val pluginName = "RegisterAgent"
  val exitOnFail = true

  def handleListen(response: RegistrationResponse)

  override def validateSession() {
    // no additional initialization required
    if (session.dsl == null) {
      throw new SessionInvalidException("session.dsl has not been initialized.")
    }
    if (LauncherPluginSession.session.selfAlias.isEmpty) {
      throw new Exception("session.selfAlias has not been initialized.")
    }
    if (LauncherPluginSession.session.agentSessionId == null){
      throw new Exception("session.agentSessionId has not been initialized.")
    }
    if (LauncherPluginSession.session.userAgentId == null) {
      throw new Exception("session.userAgentId has not been initialized.")
    }
  }

  override def execute(args: Array[String]) {
    registerAgent()
  }

  def registerAgent() {
    logger.info("Start RegisterAgent")

    val eventKey = "register"

    val registerAgentHelper = new RegisterAgentHelper() {
      def handleListen(response: RegistrationResponse) {
        logger.info("RegisterAgent Successful")
        logger.info("New AgentId = " + response.agentId + "")
        logger.info("Finish RegisterAgent")
      }
    }
    registerAgentHelper.listen(session.dsl, session.agentSessionId, eventKey)
    registerAgentHelper.request(session.dsl, session.agentSessionId, eventKey, session.BIZNETWORK_AGENT_ID, session.selfAlias)
  }
}
