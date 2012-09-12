package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util.Severity

trait LoginResponseSet {
  self:AgentHostStorePlatformAgent =>

  def listenPublicLoginResponse(cnxn: AgentCnxn) = {
    listen(_publicQ, cnxn, Channel.Security, ChannelType.Response, ChannelLevel.Public, sendPrivate(_: AgentCnxn, _: Message))
  }

}