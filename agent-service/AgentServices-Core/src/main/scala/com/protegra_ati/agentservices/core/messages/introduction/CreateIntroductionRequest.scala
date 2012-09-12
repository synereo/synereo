package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.messages._
import java.util._

case class CreateIntroductionRequest(override val eventKey: EventKey) extends Message(new Identification(), eventKey) //extends Message(eventKey) kryo workaround
with Request
{
  override def channel = Channel.Introduction

  channelRole = Some(ChannelRole.Creator)

  def this() = this(null)
}