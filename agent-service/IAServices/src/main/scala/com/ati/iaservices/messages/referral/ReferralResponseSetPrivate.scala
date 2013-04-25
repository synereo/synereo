package com.ati.iaservices.messages.referral

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util.Severity
import com.ati.iaservices.platformagents.AgentHostUIPlatformAgent

trait ReferralResponseSetPrivate
{
  self: AgentHostUIPlatformAgent =>

  def listenPrivateReferralResponses(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode() )
      listen(_privateQ, cnxn, Channel.Referral, ChannelType.Response, ChannelLevel.Private, handleReferralResponseChannel(_: AgentCnxnProxy, _: Message))
     else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Referral, ChannelType.Response, ChannelLevel.Private, handleReferralResponseChannel(cnxn, _: Message))
  }

  def handleReferralResponseChannel(cnxn: AgentCnxnProxy, msg: Message)
  {
    report("=================== HANDLE REFERRAL RESPONSE ===========, msg is :" + msg.toString + " cnxn is: " + cnxn.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[ Response ] => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
      case _ => {report("message does not generate events", Severity.Trace)}
    }

  }


}