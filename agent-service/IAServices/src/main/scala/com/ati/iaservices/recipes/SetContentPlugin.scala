package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{SetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.messages.content.{SetContentResponse, SetContentRequest}
import com.protegra_ati.agentservices.core.schema.Data

class SetContentPlugin[T <: Data] extends LauncherPluginBase {
  val pluginName = "SetContent"
  val exitOnFail = true

  var data : T = _
  var oldData : T = _

  override def validateSession(): Unit = {
    // no additional initialization required
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")

    if (data == null)
      throw new Exception("data has not been initialized.")

    if (oldData == null)
      throw new Exception("oldData has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
     setContent
  }

  def setContent(): Unit = {
    println("*************** Start SetContent ***************")

    val eventKey = "set_content"

    def requestSetContent(tag: String) = {
      val req = new SetContentRequest(new EventKey(session.agentSessionId, tag), data, oldData)
      req.targetCnxn = session.selfCnxn // TODO: IS SELF CONNECTION ALWAYS THE RIGHT ONE?
      session.ui.send(req)
    }

    def listenSetContentResponse(tag: String) = {
      session.ui.addListener(session.agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          val response : SetContentResponse = e.msg.asInstanceOf[SetContentResponse]
          println("*************** SetContentResponse ---------------")
          println(e.toString)
          println("--------------- SetContentResponse ***************")
          println("*************** Finish SetContent ***************")
        }
      })
    }

    listenSetContentResponse(eventKey)
    requestSetContent(eventKey)
  }
}
