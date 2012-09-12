package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse

class RegistrationResponseReceivedEvent(source: RegistrationResponse) extends MessageEvent[ RegistrationResponse ](source)
{
  override def trigger(listeners: List[ MessageEventAdapter ])
  {
    listeners.map {x => x.registrationResponseReceived(this)}
  }

}