package com.ati.iaservices.messages.referral

import com.protegra_ati.agentservices.core.messages._
import com.ati.iaservices.events._
import com.protegra_ati.agentservices.core.events.MessageEvent
import com.protegra_ati.agentservices.core.messages.Identification
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.messages.Message
import com.protegra_ati.agentservices.core.messages.Response
import com.protegra_ati.agentservices.core.messages.EventProducer
import com.protegra_ati.agentservices.core.messages.Channel
import com.protegra_ati.agentservices.core.events.MessageEvent
import com.ati.iaservices.events.CreateReferralResponseReceivedEvent

case class CreateReferralResponse(override val ids: Identification, override val eventKey: EventKey, val status: String) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this() = this(null, null, null)
  override def channel = Channel.Referral

  def generateEvent(): MessageEvent[_ <: Message with Response] =
  {
    new CreateReferralResponseReceivedEvent(this)
  }
}
