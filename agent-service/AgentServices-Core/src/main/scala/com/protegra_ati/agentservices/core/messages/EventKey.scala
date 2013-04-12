package com.protegra_ati.agentservices.core.messages

import java.util.UUID
import reflect.BeanProperty

case class EventKey(@BeanProperty val agentSessionId: UUID, @BeanProperty val eventTag: String)
{
  //don't want to be given an empty constructor but serializing requires it
  def this() = this (null, null)
}
