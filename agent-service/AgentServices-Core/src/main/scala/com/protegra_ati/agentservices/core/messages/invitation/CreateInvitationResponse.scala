package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._

case class CreateInvitationResponse(override val ids: Identification, override val eventKey: EventKey, val status:String)
  extends Message(ids, eventKey)
  with Response
  with EventProducer[Response]
{
  def this() = this(null, null, null)

  override def channel = Channel.Invitation
  channelRole = Some(ChannelRole.Consumer)

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new CreateInvitationResponseReceivedEvent(this)
  }

}