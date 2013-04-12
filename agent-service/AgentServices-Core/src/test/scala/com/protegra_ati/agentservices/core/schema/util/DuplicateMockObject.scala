package com.protegra_ati.agentservices.core.schema.util

import com.protegra_ati.agentservices.core.schema.MockAgentCnxnProxy

/* User: mgevantmakher
*/

case class DuplicateMockObject(val reference: MockAgentCnxnProxy, val sameReference: MockAgentCnxnProxy, val str: String)
{
  def this() = this(null, null, null)
}
