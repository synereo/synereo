package com.protegra_ati.agentservices.core.schema

import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._

import java.net.URI

case class AgentCnxnProxy(
  val src: URI,
  val label: String,
  val trgt: URI
  ) extends Data
{

  def this() = this(null, null, null)

  def toAgentCnxn() : com.protegra.agentservicesstore.AgentTS.acT.AgentCnxn = {
    new AgentCnxn(src, label, trgt)
  }

}