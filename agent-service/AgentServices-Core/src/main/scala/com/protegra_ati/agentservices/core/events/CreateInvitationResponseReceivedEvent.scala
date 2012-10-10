package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.invitation._

/* User: mtodd
*/

class CreateInvitationResponseReceivedEvent (source:CreateInvitationResponse) extends MessageEvent[CreateInvitationResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.createInvitationResponseReceived(this)}
  }
}