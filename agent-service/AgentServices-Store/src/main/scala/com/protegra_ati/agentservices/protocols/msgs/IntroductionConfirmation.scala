package com.protegra_ati.agentservices.protocols.msgs

case class IntroductionConfirmation(
    override val sessionId: Option[String],
    correlationId: String,
    accepted: Option[Boolean])
  extends ProtocolMessage {
  
  override val innerLabel = "sessionId(" + generateLabelValue(sessionId) + "),correlationId(\"" + correlationId + "\")"

  def this(sessionId: Option[String], correlationId: String) = this(sessionId, correlationId, None)
}
