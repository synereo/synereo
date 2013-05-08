package com.ati.iaservices.helpers

import com.ati.iaservices.events.{NotificationResponseReceivedEvent, MessageEventAdapter,MessageFactory}
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.protegra_ati.agentservices.core.messages.invitation.CreateInvitationRequest
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import java.util.UUID

abstract class GetNotificationResponseHelper {
  def handleListen(notificationResponse: NotificationResponseReceivedEvent)

  def request(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String, selfCnxn: AgentCnxnProxy) {
    val req = MessageFactory.createGetContentRequest(agentSessionId, tag, CreateInvitationRequest.SEARCH_ALL_PERSISTED_MESSAGE, selfCnxn)
    dsl.send(req)
  }

  def listen(dsl: AgentHostDslPlatformAgent, agentSessionId: UUID, tag: String) {
    dsl.addListener(agentSessionId, "", new MessageEventAdapter(tag) {
      override def notificationResponseReceived(e: NotificationResponseReceivedEvent) {
        handleListen(e)
      }
    })
  }
}
