package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.messages.content.{GetContentResponse, GetContentRequest}
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.List
import scala.collection.JavaConversions._
import com.rits.cloning.Cloner

class GetContentPlugin[T <: Data] extends LauncherPluginBase {
  val pluginName = "GetContent"
  val exitOnFail = true

  var queryObject : T = _

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

  def getContent(): Unit = {
    println("*************** Start GetContent ***************")

    val eventKey = "get_content"

    def requestGetContent(tag: String) = {
      val req = new GetContentRequest(new EventKey(session.agentSessionId, tag), queryObject)
      req.targetCnxn = session.selfCnxn // TODO: IS SELF CONNECTION ALWAYS THE RIGHT ONE?
      session.ui.send(req)
    }

    def listenGetContentResponse(tag: String) = {
      session.ui.addListener(session.agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
          val response : GetContentResponse = e.msg.asInstanceOf[GetContentResponse]
          println("*************** GetContentResponse ---------------")
          println(e.toString)
          println("--------------- GetContentResponse ***************")

          processData(response.data)

          println("*************** Finish GetContent ***************")
        }
      })
    }

    listenGetContentResponse(eventKey)
    requestGetContent(eventKey)
  }

  def processData(data : List[Data]) = {
    data.foreach(datum => {
      if (datum.isInstanceOf[Profile]) {
        println("*************** Found Profile Data ***************")
        session.profile = datum.asInstanceOf[Profile]
        session.oldProfile = new Cloner().deepClone(session.profile)
      }
      else {
        throw new Exception("Unsupported Data type in GetContentPlugin:processData")
      }

      // TODO: SUPPORT OTHER TYPES OF DATA
    })

  }
}
