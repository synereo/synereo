package com.ati.iaservices.recipes

import com.ati.iaservices.events.MessageFactory
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}
import java.util.UUID

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

  def request(agentSessionId: UUID, tag: String, appId: UUID, alias: String) = {
    val req = MessageFactory.createRegistrationRequest(agentSessionId, tag, appId, alias)
    session.ui.send(req)
  }

  def listen(agentSessionId: UUID, tag: String) = {
    session.ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
      {
        handleListen(e.msg)
      }
    })
  }

  def registerAgent(): Unit = {
    println("*************** Start RegisterAgent ***************")

    val eventKey = "register"

    listen(session.agentSessionId, eventKey)
    request(session.agentSessionId, eventKey, session.BIZNETWORK_AGENT_ID, session.selfAlias)
  }
}
