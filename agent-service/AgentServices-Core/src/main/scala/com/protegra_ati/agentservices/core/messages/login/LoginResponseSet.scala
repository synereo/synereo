package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util.Severity

trait LoginResponseSet {
  self:AgentHostStorePlatformAgent =>

  def listenPublicLoginResponse(cnxn: AgentCnxnProxy) = {
    listen(_publicQ, cnxn, Channel.Security, ChannelType.Response, ChannelLevel.Public, sendPrivate(_: AgentCnxnProxy, _: Message))
  }

}