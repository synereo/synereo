package com.protegra_ati.agentservices.core.messages

object ChannelRole extends Enumeration {
  type ChannelRole = Value

  //the role is set on a message based on the role of the agent that is sending the message
  val Creator = Value("Creator")
  val Consumer = Value("Consumer")
  val Store = Value("Store")
}