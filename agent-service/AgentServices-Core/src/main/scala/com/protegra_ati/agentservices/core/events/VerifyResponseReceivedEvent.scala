package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.verifier._

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class VerifyResponseReceivedEvent(source:VerifyResponse) extends MessageEvent[VerifyResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.verifyResponseReceived(this)}
  }
  
}
