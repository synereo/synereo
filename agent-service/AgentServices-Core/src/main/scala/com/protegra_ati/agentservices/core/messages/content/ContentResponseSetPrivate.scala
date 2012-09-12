package com.protegra_ati.agentservices.core.messages.content

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.events._

trait ContentResponseSetPrivate
{
  self: AgentHostUIPlatformAgent =>

  def listenPrivateContentResponse(cnxn: AgentCnxn) =
  {
    listen(_privateQ, cnxn, Channel.Content, ChannelType.Response, ChannelLevel.Private, handleContentResponseChannel(_: AgentCnxn, _: Message))
  }

  def handleContentResponseChannel(cnxn: AgentCnxn, msg: Message) =
  {
    report("entering handleContentResponse for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE CONTENT RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)


    msg match {
      case x: Message with EventProducer[Response] => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
      case _ => {}
    }
  }

}