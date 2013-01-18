package com.protegra_ati.agentservices.core.messages.invitation

/* User: jklassen
*/

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID
import com.protegra_ati.agentservices.core.events._


trait InvitationResponseSetCreatorPrivate
{
  self: AgentHostStorePlatformAgent =>

  def listenPrivateInvitationCreatorResponses(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode() )
      listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Response, ChannelLevel.Private, handleInvitationResponseChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Response, ChannelLevel.Private, handleInvitationResponseChannel(cnxn, _: Message))
  }

  def handleInvitationResponseChannel(cnxn: AgentCnxnProxy, msg: Message)
  {
    report("entering handlePrivateInvitationResponseChannel", Severity.Trace)

    msg match {

      case x: InvitationResponse => {
        //singleSend(_publicQ, x.targetCnxn, x)
        singleSend(_publicQ, x.originCnxn, x)
      }

      case _ => {
        //just putting it on the publicQ
        msg.channelLevel = Some(ChannelLevel.Public)
        send(_publicQ, msg.targetCnxn, msg)
      }
    }

    report("exiting handlePrivateInvitationRequestChannel", Severity.Trace)
  }


}