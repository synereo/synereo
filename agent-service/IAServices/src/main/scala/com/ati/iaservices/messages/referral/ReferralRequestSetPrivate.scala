package com.ati.iaservices.messages.referral

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util.Severity
import com.ati.iaservices.platformagents.AgentHostStorePlatformAgent


trait ReferralRequestSetPrivate
{
  self: AgentHostStorePlatformAgent =>

  def listenPrivateReferralRequest(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode )
      listen(_privateQ, cnxn, Channel.Referral, ChannelType.Request, ChannelLevel.Private, handlePrivateReferralRequestChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Referral, ChannelType.Request, ChannelLevel.Private, handlePrivateReferralRequestChannel(_cnxnUIStore, _: Message))
  }

  //overriding listen for hosted connections and putting broker listen logic in
  protected def handlePrivateReferralRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handlePrivate ReferralRequestChannel in ConnectionBroker", Severity.Trace)

    //just putting it on the publicQ
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)

    report("exiting handlePrivate ReferralRequestChannel in ConnectionBroker", Severity.Trace)
  }
}