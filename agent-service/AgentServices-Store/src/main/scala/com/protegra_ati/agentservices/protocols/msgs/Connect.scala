package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn

case class Connect(
    override val sessionId: Option[String],
    connectId: String,
    biCnxn: Option[PortableAgentBiCnxn])
  extends ProtocolMessage {

  def this(sessionId: Option[String], connectId: String) = this(sessionId, connectId, None)
  override val innerLabel = "sessionId(" + generateLabelValue(sessionId) + "),connectId(\"" + connectId + "\")"
}
