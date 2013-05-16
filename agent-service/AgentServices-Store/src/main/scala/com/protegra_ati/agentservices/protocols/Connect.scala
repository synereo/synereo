package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class Connect(connectId: String, requestCnxn: Option[acT.AgentCnxn], responseCnxn: Option[acT.AgentCnxn]) extends ProtocolMessage {
  def this(connectId: String) = this(connectId, None, None)
  override val innerLabel = "connectId(\"" + connectId + "\")"
}
