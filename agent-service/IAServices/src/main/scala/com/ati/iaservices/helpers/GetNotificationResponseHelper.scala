package com.ati.iaservices.helpers

import com.ati.iaservices.messages.notification.NotificationResponse
import com.ati.iaservices.events.{NotificationResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import java.util.UUID
import scala.collection.JavaConversions._

abstract class GetNotificationResponseHelper {
  def handleListen(notificationResponse: NotificationResponseReceivedEvent) {}

  def listen(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) {
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def notificationResponseReceived(e: NotificationResponseReceivedEvent) {
        handleListen(e)
      }
    })
  }
}
