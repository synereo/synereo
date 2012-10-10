package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.invitation._
import com.protegra_ati.agentservices.core.messages.introduction.CreateIntroductionResponse

/* User: mtodd
*/

class CreateIntroductionResponseReceivedEvent (source:CreateIntroductionResponse) extends MessageEvent[CreateIntroductionResponse](source) {

  override def trigger(listeners:List[MessageEventAdapter]){
    listeners.map {x => x.createIntroductionResponseReceived(this)}
  }
}