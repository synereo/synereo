package com.protegra_ati.agentservices.protocols.msgs

case class BeginIntroductionResponse(
    override val sessionId: Option[String],
    override val responseId: String,
    accepted: Option[Boolean],
    aRejectReason: Option[String],
    bRejectReason: Option[String])
  extends ProtocolResponseMessage {

  def this(sessionId: Option[String], responseId: String) = this(sessionId, responseId, None, None, None)
}
