package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.core.messages.{EventKey, Identification}
import com.protegra_ati.agentservices.core.messages.content.{GetContentResponse, GetContentRequest, SetSelfContentRequest}
import com.ati.iaservices.recipes.LauncherPluginSession._
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.ati.iaservices.helpers.ConnectToAllHelper

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 10/04/13
 * Time: 10:35 AM
 * To change this template use File | Settings | File Templates.
 */
class ConnectToAllPlugin  extends LauncherPluginBase {
  val pluginName = "ConnectToAll"
  val exitOnFail = true

  override def validateSession() = {
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")

    if (session.agentSessionId == null)
      throw new Exception("agentSessionId has not been initialized.")

    if (session.selfAlias == null)
      throw new Exception("selfAlias has not been initialized.")

    if (session.userAgentId == null)
      throw new Exception("userAgentId has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
    val helper = new ConnectToAllHelper() {
      def handleConnectionsCompleted() {
        println("*************** ConnectToAll Successful ***************")
        println("*************** Finish ConnectToAll ***************")
      }
    }
    helper.connectToAll(session.ui, session.selfCnxn, session.agentSessionId, session.selfAlias, session.userAgentId)
  }
}
