package com.ati.iaservices.messages.notification

import com.protegra_ati.agentservices.core.events.MessageEvent
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema.Data
import com.ati.iaservices.events.NotificationResponseReceivedEvent

case class NotificationResponse(override val ids: Identification,
            override val eventKey: EventKey,
            data: java.util.List[Data]) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this () = this(null,null,null)
  def this (eventKey: EventKey, data: java.util.List[Data] ) = this(new Identification(), eventKey, data)
  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Notification


  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new NotificationResponseReceivedEvent(this)
  }
}
