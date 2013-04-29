package com.protegra_ati.agentservices.core.messages

object ChannelType extends Enumeration {
  type ChannelType = Value

  val Request = Value("Request")
  val Response = Value("Response")
  val Notification = Value("Notification")
}