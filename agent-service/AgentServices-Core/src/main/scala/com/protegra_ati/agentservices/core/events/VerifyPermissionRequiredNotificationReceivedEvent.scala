package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.verifier._

/* User: mtodd
*/

class VerifyPermissionRequiredNotificationReceivedEvent(source:VerifyPermissionRequiredNotification) extends MessageEvent[VerifyPermissionRequiredNotification](source) {
   override def triggerEvent(adapter: MessageEventAdapter) = {
    adapter.verifyPermissionRequiredNotificationReceived(this)
  }

}