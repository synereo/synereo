package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.ati.iaservices.helpers.ConnectToAllHelper
import com.protegra_ati.agentservices.store.util.LogConfiguration._

class ConnectToAllPlugin extends LauncherPluginBase {
  val pluginName = "ConnectToAll"
  val exitOnFail = true

  override def validateSession() {
    if (session.dsl == null) {
      throw new Exception("session.dsl has not been initialized.")
    }

    if (session.agentSessionId == null) {
      throw new Exception("agentSessionId has not been initialized.")
    }

    if (session.selfAlias == null) {
      throw new Exception("selfAlias has not been initialized.")
    }

    if (session.userAgentId == null) {
      throw new Exception("userAgentId has not been initialized.")
    }
  }

  override def execute(args: Array[String]) {
    val helper = new ConnectToAllHelper() {
      def handleConnectionsCompleted() {
        logger.info("*************** ConnectToAll Successful ***************")
        logger.info("*************** Finish ConnectToAll ***************")
      }
    }

    helper.connectToAll(session.dsl, session.selfCnxn, session.agentSessionId, session.selfAlias, session.BIZNETWORK_AGENT_ID)
  }
}
