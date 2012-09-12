package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._

case class SetSelfContentResponse(override val ids: Identification, override val eventKey: EventKey, isContentSet: Boolean) extends Message(ids, eventKey)
with Response
{
  def this() = this (null,null,false)
  channelLevel = Some(ChannelLevel.Single)
 // override val channelRole = Some(ChannelRole.Creator)

  override def channel = Channel.Content
}