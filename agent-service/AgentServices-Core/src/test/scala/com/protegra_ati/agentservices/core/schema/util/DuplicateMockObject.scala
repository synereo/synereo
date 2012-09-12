package com.protegra_ati.agentservices.core.schema.util

import com.protegra_ati.agentservices.core.schema.MockAgentCnxn

/* User: mgevantmakher
*/

case class DuplicateMockObject(val reference: MockAgentCnxn, val sameReference: MockAgentCnxn, val str: String)
{
  def this() = this(null, null, null)
}
