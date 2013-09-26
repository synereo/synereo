package com.protegra_ati.agentservices.protocols.msgs

case class IntroductionResponse(
    override val sessionId: Option[String],
    override val responseId: String,
    accepted: Option[Boolean],
    cnxnName: Option[String],
    rejectReason: Option[String],
    connectId: Option[String])
  extends ProtocolResponseMessage {

  def this(sessionId: Option[String], responseId: String) = this(sessionId, responseId, None, None, None, None)
}
