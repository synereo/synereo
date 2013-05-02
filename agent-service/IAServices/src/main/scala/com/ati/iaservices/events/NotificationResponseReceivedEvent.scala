package com.ati.iaservices.events

import com.ati.iaservices.messages.notification.NotificationResponse
import com.protegra_ati.agentservices.core.events.MessageEvent
import com.protegra_ati.agentservices.core.events.{MessageEventAdapter => CoreMessageEventAdapter}

class NotificationResponseReceivedEvent(source: NotificationResponse) extends MessageEvent[NotificationResponse](source)
{
  override def triggerEvent(adapter: CoreMessageEventAdapter) =
  {
    adapter match {
      case x: MessageEventAdapter =>
      {
        x.notificationResponseReceived(this)
      }
      case _ => {}
    }
  }
}
