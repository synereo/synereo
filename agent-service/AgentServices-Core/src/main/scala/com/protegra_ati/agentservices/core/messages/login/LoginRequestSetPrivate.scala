package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.login._

trait LoginRequestSetPrivate {
  self:AgentHostStorePlatformAgent =>

  def listenPrivateLoginRequest(cnxn: AgentCnxn) = {
    listen(_privateQ, _cnxnUIStore, Channel.Security, ChannelType.Request, ChannelLevel.Private, handlePrivateSecurityRequestChannel(_: AgentCnxn, _: Message))
  }

  private def handlePrivateSecurityRequestChannel(cnxn: AgentCnxn, msg: Message) =
  {
    //just putting it on the publicQ
    //see the comment in  handlePublicContentResponseChannel(cnxn: AgentCnxn, msg: Message)
    //concerning the handling of the ChannelLevel
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)
  }

}