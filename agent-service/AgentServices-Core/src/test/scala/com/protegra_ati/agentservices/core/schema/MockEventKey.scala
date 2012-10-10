package com.protegra_ati.agentservices.core.schema

import java.util.UUID
import reflect.BeanProperty

/* User: mgevantmakher
*/

case class MockEventKey(@BeanProperty val agentSessionId: UUID, @BeanProperty val eventTag: String)
{
  //don't want to be given an empty constructor but serializing requires it
  def this() = this(null, null)
}
