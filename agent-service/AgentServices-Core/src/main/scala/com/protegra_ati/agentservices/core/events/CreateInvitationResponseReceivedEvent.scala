package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.invitation._

/* User: mtodd
*/

class CreateInvitationResponseReceivedEvent (source:CreateInvitationResponse) extends MessageEvent[CreateInvitationResponse](source) {

  override def triggerEvent(adapter: MessageEventAdapter) = {
    adapter.createInvitationResponseReceived(this)
  }
}