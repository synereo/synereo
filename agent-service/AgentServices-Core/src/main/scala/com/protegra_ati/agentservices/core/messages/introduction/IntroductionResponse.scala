package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.messages._


case class IntroductionResponse(override val ids: Identification, override val eventKey: EventKey, val introductionId: String, val accept: Boolean) extends Message(ids, eventKey)
with Response
{
  override def channel = Channel.Introduction

  channelRole = Some(ChannelRole.Creator)
  channelLevel = Some(ChannelLevel.Public)
  def this() = this(null, null, null, false)
}