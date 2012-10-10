package com.protegra_ati.agentservices.core.messages

/* User: mtodd
*/

object ChannelLevel extends Enumeration("Public", "Private", "Single")
{
  type ChannelLevel = Value
  val Public, Private, Single = Value
}

