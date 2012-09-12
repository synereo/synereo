package com.protegra_ati.agentservices.core.messages.content

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._

trait ContentResponseSet {
  self:AgentHostStorePlatformAgent =>

  def listenPublicContentResponse(cnxn: AgentCnxn) =
  {
    listen(_publicQ, cnxn, Channel.Content, ChannelType.Response, ChannelLevel.Public, sendPrivate(_: AgentCnxn, _: Message))
  }
}