package com.protegra_ati.agentservices.protocols.msgs

case class IntroductionResponse(
    override val sessionId: Option[String],
    override val correlationId: String,
    accepted: Option[Boolean],
    connectId: Option[String])
  extends ProtocolResponseMessage {

  def this(sessionId: Option[String], correlationId: String) = this(sessionId, correlationId, None, None)
}
