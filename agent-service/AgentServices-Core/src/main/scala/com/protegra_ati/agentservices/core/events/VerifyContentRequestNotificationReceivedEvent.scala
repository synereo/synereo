package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.verifier._

/* User: mtodd
*/

class VerifyContentRequestNotificationReceivedEvent(source:VerifyContentRequestNotification) extends MessageEvent[VerifyContentRequestNotification](source) {
   override def triggerEvent(adapter: MessageEventAdapter) = {
    adapter.verifyContentRequestNotificationReceived(this)
  }

}