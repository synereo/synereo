package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.events._

case class SetContentPersistedResponse(override val ids: Identification, override val eventKey: EventKey, @BeanProperty persistedMessageData:CompositeData[PersistedRequest]) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this() =this (null, null, null)
  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Content

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new SetContentPersistedResponseReceivedEvent(this)
  }
}