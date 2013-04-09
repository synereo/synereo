package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._

case class VerifyContentRequestNotification(override val eventKey: EventKey) extends Message(eventKey) with Notification {
  def this() = this(null)

  override def channel = Channel.Verify

  def generateEvent(): MessageEvent[ _ <: Message with Notification ] = {
    new VerifyContentRequestNotificationReceivedEvent(this)
  }
}