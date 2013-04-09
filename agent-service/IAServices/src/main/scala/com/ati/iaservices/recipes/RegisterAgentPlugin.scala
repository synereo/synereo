package com.ati.iaservices.recipes

import java.util.UUID
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.core.messages.admin.{RegistrationResponse, RegistrationRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent, AgentHostUIPlatformAgent}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 10:02 AM
 * To change this template use File | Settings | File Templates.
 */
class RegisterAgentPlugin extends LauncherPluginBase {
  val pluginName = "RegisterAgent"
  val exitOnFail = true

  override def validateSession(): Unit = {
    // no additional initialization required
    if (LauncherPluginSession.session.ui == null)
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

    val eventKey = "registration"

    def requestRegistration(tag: String) = {
      val req = new RegistrationRequest(
        new EventKey(LauncherPluginSession.session.agentSessionId, tag),
        LauncherPluginSession.session.BIZNETWORK_AGENT_ID,
        LauncherPluginSession.session.selfAlias)
      req.targetCnxn = LauncherPluginSession.session.selfCnxn
      LauncherPluginSession.session.ui.send(req)
    }

    def listenRegistrationResponse(tag: String) = {
      LauncherPluginSession.session.ui.addListener(LauncherPluginSession.session.agentSessionId, "", new MessageEventAdapter(tag)
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
      })
    }

    listenRegistrationResponse(eventKey)
    requestRegistration(eventKey)
  }
}
