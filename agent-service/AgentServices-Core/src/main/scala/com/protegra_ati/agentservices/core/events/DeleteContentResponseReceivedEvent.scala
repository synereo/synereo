package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.content._

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class DeleteContentResponseReceivedEvent(source:DeleteContentResponse) extends MessageEvent[DeleteContentResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.deleteContentResponseReceived(this)}
  }
  
}
