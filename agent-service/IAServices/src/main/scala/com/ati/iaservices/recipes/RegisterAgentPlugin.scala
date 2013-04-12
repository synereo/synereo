package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.ati.iaservices.helpers.RegisterAgentHelper

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 10:02 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class RegisterAgentPlugin extends LauncherPluginBase {
  val pluginName = "RegisterAgent"
  val exitOnFail = true

  def handleListen(response: RegistrationResponse)

  override def validateSession(): Unit = {
    // no additional initialization required
    if (session.ui == null)
      throw new SessionInvalidException("session.ui has not been initialized.")
    if (LauncherPluginSession.session.selfAlias.isEmpty())
      throw new Exception("session.selfAlias has not been initialized.")
    if (LauncherPluginSession.session.agentSessionId == null)
      throw new Exception("session.agentSessionId has not been initialized.")
    if (LauncherPluginSession.session.userAgentId == null)
      throw new Exception("session.userAgentId has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
    registerAgent
  }

  def registerAgent(): Unit = {
    println("*************** Start RegisterAgent ***************")

    val eventKey = "register"

    val registerAgentHelper = new RegisterAgentHelper() {
      def handleListen(response: RegistrationResponse) = {
        println("*************** RegisterAgent Successful ***************")
        println("*************** New AgentId = " + response.agentId + " ***************")
        println("*************** Finish RegisterAgent ***************")
        session.selfCnxn = response.connSelf.writeCnxn
        session.selfAlias = response.connSelf.alias
        session.userAgentId = response.agentId
      }
    }
    registerAgentHelper.listen(session.ui, session.agentSessionId, eventKey)
    registerAgentHelper.request(session.ui, session.agentSessionId, eventKey, session.BIZNETWORK_AGENT_ID, session.selfAlias)
  }
}
