package com.protegra_ati.agentservices.core.messages.admin

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._
import java.util.UUID
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.Connection

case class RegistrationResponse(override val ids: Identification, override val eventKey: EventKey, @BeanProperty agentId: UUID, @BeanProperty connSelf: Connection, @BeanProperty invitationConnectionId: String) extends Message(ids, eventKey)
with Response
with EventProducer[ Response ]
{
  //this only happens on private servers
  channelLevel = Some(ChannelLevel.Private)

  def this() = this(null, null, null, null, null)

  override def channel = Channel.Admin

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new RegistrationResponseReceivedEvent(this)
  }
}