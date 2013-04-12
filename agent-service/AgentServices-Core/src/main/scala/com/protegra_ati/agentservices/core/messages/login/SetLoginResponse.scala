package com.protegra_ati.agentservices.core.messages.login

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import java.util._
import com.protegra_ati.agentservices.core.events._

case class SetLoginResponse(override val ids: Identification, override val eventKey: EventKey) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this() = this(null, null)

  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Security

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new SetLoginResponseReceivedEvent(this)
  }
}