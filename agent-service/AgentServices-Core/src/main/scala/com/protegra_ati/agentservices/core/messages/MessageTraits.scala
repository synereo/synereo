package com.protegra_ati.agentservices.core.messages

import java.util.UUID
import com.protegra_ati.agentservices.core.events._

//not all requests will generate notifications
trait Request {
  self: Message =>

  override def channelType = ChannelType.Request

  def getResponseChannelKey: String =
  {
    var channelKey = channel.toString + ChannelType.Response.toString + channelLevel.getOrElse(ChannelLevel.Private).toString
    channelKey += "(\"" + ids.conversationId.toString + "\")"
    channelKey
  }
}

//not all responses generate events
trait Response {
  self: Message =>
  //default response channel level to public
    channelLevel = Some(ChannelLevel.Public)
    override def channelType = ChannelType.Response
}

//all notifications must generate events
trait Notification extends EventProducer[Notification] {
  self: Message =>

  override def channelType = ChannelType.Notification
}

trait EventProducer[T] {
  self: Message with T =>

  def generateEvent(): MessageEvent[ _ <: Message with T ]
}

trait NotificationProducer {
  self: Message =>

  def generateNotification(key: EventKey): Message with Notification
}


