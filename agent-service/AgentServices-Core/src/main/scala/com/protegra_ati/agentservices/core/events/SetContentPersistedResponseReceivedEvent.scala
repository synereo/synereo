package com.protegra_ati.agentservices.core.events

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.protegra_ati.agentservices.core.messages.content._

class SetContentPersistedResponseReceivedEvent(source:SetContentPersistedResponse) extends MessageEvent[SetContentPersistedResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.setContentPersistedResponseReceived(this)}
  }
  
}
