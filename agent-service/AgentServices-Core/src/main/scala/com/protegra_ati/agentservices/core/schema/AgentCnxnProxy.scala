package com.protegra_ati.agentservices.core.schema

import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._

import java.net.URI
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class AgentCnxnProxy(
  val src: URI,
  val label: String,
  val trgt: URI
  ) extends UseKryoSerialization
{

  def this() = this(null, null, null)

  //STRESS TODO apply the flyweight pattern (http://en.wikipedia.org/wiki/Flyweight_pattern) to control the number of objects of this type
  def toAgentCnxn(): com.protegra.agentservicesstore.usage.AgentKVDBScope.acT.AgentCnxn =
  {
    new AgentCnxn(src, label, trgt)
  }

  def getExchangeKey(): String =
  {
    toAgentCnxn()._symmIdCode.toString()
  }

}