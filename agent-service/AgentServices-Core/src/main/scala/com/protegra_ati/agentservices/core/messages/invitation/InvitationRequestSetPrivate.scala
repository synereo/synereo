package com.protegra_ati.agentservices.core.messages.invitation

/* User: jklassen
*/

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID


trait InvitationRequestSetPrivate
{
  self: AgentHostStorePlatformAgent =>

  def listenPrivateInvitationRequest(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode ) {
      listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(_: AgentCnxnProxy, _: Message))
      listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(_: AgentCnxnProxy, _: Message))
    }
    else {
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(cnxn, _: Message))
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(cnxn, _: Message))
    }
  }

  //overriding listen for hosted connections and putting broker listen logic in
  protected def handlePrivateInvitationRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handlePrivateInvitationRequestChannel in ConnectionBroker", Severity.Trace)

    //just putting it on the publicQ
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)

    report("exiting handlePrivateInvitationRequestChannel in ConnectionBroker", Severity.Trace)
  }
}