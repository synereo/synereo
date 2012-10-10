package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._

case class CreateIntroductionResponse(override val ids: Identification, override val eventKey: EventKey, val status: String) extends Message(ids, eventKey)
with Response
with EventProducer[ Response ]
{
  override def channel = Channel.Introduction

  channelRole = Some(ChannelRole.Consumer)
  def this() = this(null, null, null)

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new CreateIntroductionResponseReceivedEvent(this)
  }

}