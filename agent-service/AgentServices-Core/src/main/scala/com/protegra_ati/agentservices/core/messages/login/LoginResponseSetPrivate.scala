package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.login._
import com.protegra_ati.agentservices.core.events._
import com.protegra.agentservicesstore.util.Severity

trait LoginResponseSetPrivate {
  self:AgentHostUIPlatformAgent =>

  def listenPrivateLoginResponse(cnxn: AgentCnxnProxy) = {
    listen(_privateQ, cnxn, Channel.Security, ChannelType.Response, ChannelLevel.Private, handleSecurityResponseChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handleSecurityResponseChannel( cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleSecurityResponse for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE SECURITY RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[Response] => {
        report("in handleSecurityResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
//      case x: SetLoginResponse => {
//        report("set login response", Severity.Trace)
//        //the ui doesnt care about the get/fetch distinction it means fetch
//        val event = new SetLoginResponseReceivedEvent(x)
//        event.msg = x;
//        report("in handleSecurityResponseResponse triggering event for setLoginResponse", Severity.Trace)
//        triggerEvent(event)
//      }
      case _ => {}
    }

  }
}