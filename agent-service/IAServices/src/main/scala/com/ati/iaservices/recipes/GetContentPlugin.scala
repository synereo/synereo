package com.ati.iaservices.recipes

import com.ati.iaservices.events.MessageFactory
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Data}
import java.util.UUID
import scala.collection.JavaConversions._

abstract class GetContentPlugin[T <: Data] extends LauncherPluginBase {
  val pluginName = "GetContent"
  val exitOnFail = true

  var queryObject : T = _

  def handleListen(data: T)

  override def validateSession(): Unit = {
    // no additional initialization required
    if (session.ui == null)
      throw new Exception("session.ui has not been initialized.")

    if (queryObject == null)
      throw new Exception("queryObject has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
     getContent
  }

  def request(agentSessionId : UUID, tag: String, queryObject : Data, target : AgentCnxnProxy) = {
    val req = MessageFactory.createGetContentRequest(agentSessionId, tag, queryObject, target)
    session.ui.send(req)
  }

  def listen(agentSessionId : UUID, tag: String) = {
    session.ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        for (datum <- e.getMsg.data) {
          handleListen(datum.asInstanceOf[T])
        }
      }
    })
  }

  def getContent(): Unit = {
    println("*************** Start GetContent ***************")

    val eventKey = "get_content"

    listen(session.agentSessionId, eventKey)
    request(session.agentSessionId, eventKey, queryObject, session.selfCnxn)
  }
}
