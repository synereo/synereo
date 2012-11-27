package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.login._

/* User: mtodd
*/

class SetLoginResponseReceivedEvent (source:SetLoginResponse) extends MessageEvent[SetLoginResponse](source) {
   override def triggerEvent(adapter: MessageEventAdapter) = {
    adapter.setLoginResponseReceived(this)
  }

}