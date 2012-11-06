package com.protegra_ati.agentservices.core.messages.introduction

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._


trait IntroductionRequestSetPrivate
{
  self: AgentHostStorePlatformAgent =>

  def listenPrivateIntroductionRequest(cnxn: AgentCnxnProxy) =
  {
    listen(_privateQ, cnxn, Channel.Introduction, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Private, handlePrivateIntroductionRequestChannel(_: AgentCnxnProxy, _: Message))
    listen(_privateQ, cnxn, Channel.Introduction, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Private, handlePrivateIntroductionRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  //overriding listen for hosted connections and putting broker listen logic in
  protected def handlePrivateIntroductionRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handlePrivateIntroductionRequestChannel in ConnectionBroker", Severity.Trace)

    //just putting it on the publicQ
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)

    report("exiting handlePrivateIntroductionRequestChannel in ConnectionBroker", Severity.Trace)
  }
}