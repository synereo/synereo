package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class Connect(
    override val sessionId: Option[String],
    connectId: String,
    cnxnName: Option[String],
    readCnxn: Option[acT.AgentCnxn],
    WriteCnxn: Option[acT.AgentCnxn])
  extends ProtocolMessage {

  def this(sessionId: Option[String], connectId: String) = this(sessionId, connectId, None, None, None)
  override val innerLabel = "sessionId(" + generateLabelValue(sessionId) + "),connectId(\"" + connectId + "\")"
}
