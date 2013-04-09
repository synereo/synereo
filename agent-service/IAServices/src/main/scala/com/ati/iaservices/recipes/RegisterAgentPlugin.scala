package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.admin.{RegistrationResponse, RegistrationRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{RegistrationResponseReceivedEvent, MessageEventAdapter}

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
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
     registerAgent
  }

  def registerAgent(): Unit = {
    println("*************** Start RegisterAgent ***************")

    val eventKey = "registration"

    def requestRegistration(tag: String) = {
      val req = new RegistrationRequest(new EventKey(session.agentSessionId, tag),session.BIZNETWORK_AGENT_ID, session.selfAlias)
      session.ui.send(req)
    }

    def listenRegistrationResponse(tag: String) = {
      session.ui.addListener(session.agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
        {
          val response : RegistrationResponse = e.msg.asInstanceOf[RegistrationResponse]
          val newAgentId = response.agentId
          session.userAgentId = response.agentId
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
