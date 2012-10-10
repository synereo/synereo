package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.messages._

case class IntroductionRequest(override val ids: Identification, override val eventKey: EventKey, val introductionId: String, val alias: String)
  extends Message(ids, eventKey) with
Request
{
  override def channel = Channel.Introduction

  channelRole = Some(ChannelRole.Consumer)
  channelLevel = Some(ChannelLevel.Public)

  def this() = this(null, null, null, null)

  override def getResponseChannelKey: String =
  {
    var channelKey = channel.toString + ChannelRole.Creator.toString + ChannelType.Response.toString + ChannelLevel.Single.toString
    channelKey += "(\"" + ids.conversationId.toString + "\")"
    channelKey
  }
}