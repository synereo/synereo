package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.login._

trait LoginRequestSetPrivate {
  self:AgentHostStorePlatformAgent =>

  def listenPrivateLoginRequest(cnxn: AgentCnxnProxy) = {
    if ( isPrivateKVDBNetworkMode() )
      listen(_privateQ, cnxn, Channel.Security, ChannelType.Request, ChannelLevel.Private, handlePrivateSecurityRequestChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Security, ChannelType.Request, ChannelLevel.Private, handlePrivateSecurityRequestChannel(cnxn, _: Message))

  }

  protected def handlePrivateSecurityRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    //just putting it on the publicQ
    //see the comment in  handlePublicContentResponseChannel(cnxn: AgentCnxnProxy, msg: Message)
    //concerning the handling of the ChannelLevel
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)
  }

}