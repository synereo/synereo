package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.verifier._

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class VerifyContentResponseReceivedEvent(source:VerifyContentResponse) extends MessageEvent[VerifyContentResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.verifyContentResponseReceived(this)}
  }
  
}
