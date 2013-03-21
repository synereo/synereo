package com.protegra_ati.agentservices.core.messages.invitation

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util._

trait InvitationResponseSetConsumerPrivate
{
  self: AgentHostUIPlatformAgent =>

  def listenPrivateInvitationConsumerResponses(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode() )
      listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Response, ChannelLevel.Private, handleInvitationResponseChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Response, ChannelLevel.Private, handleInvitationResponseChannel(cnxn, _: Message))
  }

  def handleInvitationResponseChannel(cnxn: AgentCnxnProxy, msg: Message)
  {
    report("=================== HANDLE INVITATION RESPONSE ===========, msg is :" + msg.toString + " cnxn is: " + cnxn.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[ Response ] => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
      case _ => {report("message does not generate events", Severity.Trace)}
    }

  }


}