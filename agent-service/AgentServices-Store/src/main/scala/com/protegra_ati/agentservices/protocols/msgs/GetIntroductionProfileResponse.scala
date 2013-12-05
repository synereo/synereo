package com.protegra_ati.agentservices.protocols.msgs

case class GetIntroductionProfileResponse(
    override val sessionId: Option[String],
    override val correlationId: String,
    val profileData: Option[String])
  extends ProtocolResponseMessage {

  def this(sessionId: Option[String], correlationId: String) = this(sessionId, correlationId, None)
}
