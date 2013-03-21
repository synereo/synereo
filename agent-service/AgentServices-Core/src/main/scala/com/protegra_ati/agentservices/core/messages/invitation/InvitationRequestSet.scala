package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._

trait InvitationRequestSet
  extends InvitationRequestSetCreator
  with InvitationRequestSetConsumer  {
  self:AgentHostStorePlatformAgent =>

}