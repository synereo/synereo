package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.verifier._

/* User: mtodd
*/

class SelectVerifierRequestNotificationReceivedEvent(source:SelectVerifierRequestNotification) extends MessageEvent[SelectVerifierRequestNotification](source) {
   override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.selectVerifierRequestNotificationReceivedEvent(this)}
  }

}