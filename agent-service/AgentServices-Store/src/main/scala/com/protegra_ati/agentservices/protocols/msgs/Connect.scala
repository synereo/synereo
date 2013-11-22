package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class Connect(
    override val sessionId: Option[String],
    connectId: String,
    biCnxn: Option[acT.AgentBiCnxn])
  extends ProtocolMessage {

  def this(sessionId: Option[String], connectId: String) = this(sessionId, connectId, None)
  override val innerLabel = "sessionId(" + generateLabelValue(sessionId) + "),connectId(\"" + connectId + "\")"
}
