package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.helpers.ConnectToAllHelper

class ConnectToAllPlugin extends LauncherPluginBase {
  val pluginName = "ConnectToAll"
  val exitOnFail = true

  override def validateSession() {
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")

    if (session.agentSessionId == null)
      throw new Exception("agentSessionId has not been initialized.")

    if (session.selfAlias == null)
      throw new Exception("selfAlias has not been initialized.")

    if (session.userAgentId == null)
      throw new Exception("userAgentId has not been initialized.")
  }

  override def execute(args: Array[String]) {
    val helper = new ConnectToAllHelper() {
      def handleConnectionsCompleted() {
        println("*************** ConnectToAll Successful ***************")
        println("*************** Finish ConnectToAll ***************")
      }
    }
    helper.connectToAll(session.ui, session.selfCnxn, session.agentSessionId, session.selfAlias)
  }
}
