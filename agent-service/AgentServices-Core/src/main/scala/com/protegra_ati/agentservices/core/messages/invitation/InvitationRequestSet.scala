package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.AgentTS.acT._

trait InvitationRequestSet
  extends InvitationRequestSetCreator
  with InvitationRequestSetConsumer  {
  self:AgentHostStorePlatformAgent =>

}