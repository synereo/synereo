package com.protegra_ati.agentservices.core.schema.util

import com.protegra_ati.agentservices.core.schema.MockAgentCnxn
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

/* User: mgevantmakher
*/

case class DuplicateMockObject(val reference: MockAgentCnxn, val sameReference: MockAgentCnxn, val str: String) extends UseKryoSerialization
{
  def this() = this(null, null, null)
}
