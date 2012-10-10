package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.platformagents._

trait IntroductionRequestSet
  extends IntroductionRequestSetCreator
  with IntroductionRequestSetConsumer
{
  self: AgentHostStorePlatformAgent =>

}