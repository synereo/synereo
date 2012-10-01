package com.protegra_ati.agentservices.core.schema.util

import com.protegra_ati.agentservices.core.schema.MockAgentCnxnProxy
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

/* User: mgevantmakher
*/

case class DuplicateMockObject(val reference: MockAgentCnxnProxy, val sameReference: MockAgentCnxnProxy, val str: String) extends UseKryoSerialization
{
  def this() = this(null, null, null)
}
