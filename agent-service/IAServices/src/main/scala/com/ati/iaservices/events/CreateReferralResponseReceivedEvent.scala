package com.ati.iaservices.events

import com.protegra_ati.agentservices.core.events.MessageEvent
import com.protegra_ati.agentservices.core.events.{MessageEventAdapter => CoreMessageEventAdapter}
import com.ati.iaservices.messages.referral.CreateReferralResponse

class CreateReferralResponseReceivedEvent(source: CreateReferralResponse) extends MessageEvent[CreateReferralResponse](source)
{
  override def triggerEvent(adapter: CoreMessageEventAdapter) =
  {
    adapter match {
      case x: MessageEventAdapter =>
      {
        x.createReferralResponseReceived(this)
      }
      case _ => {}
    }
  }
}
