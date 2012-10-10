package com.protegra_ati.agentservices.core.messages

import java.util.UUID
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class EventKey(@BeanProperty val agentSessionId: UUID, @BeanProperty val eventTag: String)  extends UseKryoSerialization
{
  //don't want to be given an empty constructor but serializing requires it
  def this() = this (null, null)
}
