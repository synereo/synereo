package com.ati.iaservices.messages.referral

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.ati.iaservices.platformagents.AgentHostStorePlatformAgent

trait ReferralResponseSet
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicReferralResponses(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Referral, ChannelType.Response, ChannelLevel.Public, sendPrivate(_: AgentCnxnProxy, _: Message))
  }
}