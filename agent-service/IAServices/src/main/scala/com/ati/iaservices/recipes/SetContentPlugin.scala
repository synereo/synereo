package com.ati.iaservices.recipes

import com.ati.iaservices.events.MessageFactory
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import java.util.UUID

abstract class SetContentPlugin[T <: Data] extends LauncherPluginBase {
  val pluginName = "SetContent"
  val exitOnFail = true

  var data : T = _
  var oldData : T = _

  def handleListen(data: T)

  override def validateSession(): Unit = {
    // no additional initialization required
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")

    if (data == null)
      throw new Exception("data has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
     setContent
  }

  def request(agentSessionId : UUID, tag: String, data : Data, target : AgentCnxnProxy) = {
    val req = MessageFactory.createSetContentRequest(agentSessionId, tag, data, target)
    session.ui.send(req)
  }

  def listen(agentSessionId: UUID, tag: String) = {
    session.ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
      {
        handleListen(e.getMsg.data.asInstanceOf[T])
      }
    })
  }

  def setContent(): Unit = {
    println("*************** Start SetContent ***************")

    val eventKey = "set_content"

    listen(session.agentSessionId, eventKey)
    request(session.agentSessionId, eventKey, data, session.selfCnxn)
  }
}
