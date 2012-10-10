package com.protegra_ati.agentservices.core.messages.content

import java.util.UUID

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._

case class GetContentAuthorizationRequiredNotification(override val eventKey: EventKey) extends Message(new Identification(), eventKey) //extends Message(eventKey) -- kryo
with Notification {
  def this() = this(null)
     override def channel = Channel.Content

     def generateEvent(): MessageEvent[ _ <: Message with Notification ] = {
       new GetContentAuthorizationRequiredNotificationReceivedEvent(this)
     }
}