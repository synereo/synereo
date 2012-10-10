package com.protegra_ati.agentservices.core.messages

//string list for Enum constructor is required for serialization
//val must be kept in same order as enum constructor
object ChannelRole extends Enumeration("Creator", "Consumer", "Store")
{
  type ChannelRole = Value

  //the role is set on a message based on the role of the agent that is sending the message
  val Creator, Consumer, Store = Value
}