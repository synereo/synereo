package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.login._

/* User: mtodd
*/

class SetLoginResponseReceivedEvent (source:SetLoginResponse) extends MessageEvent[SetLoginResponse](source) {
   override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.setLoginResponseReceived(this)}
  }

}