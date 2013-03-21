package com.protegra_ati.agentservices.core.messages.verifier

/* User: jviolago
*/


import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util.Severity
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.events.{VerifyContentResponseReceivedEvent, GetClaimResponseReceivedEvent, VerifyResponseReceivedEvent}

trait VerifierResponseSetPrivate {
  self:AgentHostUIPlatformAgent =>

  def listenPrivateVerifierResponse(cnxn: AgentCnxnProxy) = {
//     listen(_privateQ, cnxn, Channel.Verify, ChannelType.Response, ChannelLevel.Private, handleVerifyResponseChannel(_: AgentCnxnProxy, _: Message))
  }

  def handleVerifyResponseChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleVerifyResponseChannel for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE VERIFY RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[Response] => {
        report("in handleVerifyResponseChannel triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
//      case x: VerifyResponse => {
//        val event = new VerifyResponseReceivedEvent(x)
//        event.msg = x;
//        report("in handleVerifyResponse triggering event for getVerifyResponse", Severity.Trace)
//        triggerEvent(event)
//      }
//      case x: GetClaimResponse => {
//        val event = new GetClaimResponseReceivedEvent(x)
//        event.msg = x;
//        report("in handleVerifyResponse triggering event for claim response", Severity.Trace)
//        triggerEvent(event)
//      }
//      case x: VerifyContentResponse => {
//        val event = new VerifyContentResponseReceivedEvent(x)
//        event.msg = x;
//        report("in handleVerifyResponse triggering event for verify content response", Severity.Trace)
//        triggerEvent(event)
//      }
      case _ => {}
    }
  }

}