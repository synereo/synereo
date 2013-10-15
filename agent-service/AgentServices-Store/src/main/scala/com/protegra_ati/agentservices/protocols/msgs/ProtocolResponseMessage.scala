package com.protegra_ati.agentservices.protocols.msgs

trait ProtocolResponseMessage extends ProtocolMessage {
  val correlationId: String
  override val innerLabel = "sessionId(" + generateLabelValue(sessionId) + "),correlationId(\"" + correlationId + "\")"
}
