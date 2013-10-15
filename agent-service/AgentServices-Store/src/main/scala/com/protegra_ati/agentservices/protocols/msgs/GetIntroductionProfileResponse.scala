package com.protegra_ati.agentservices.protocols.msgs

// TODO: Add introduction profile to message
case class GetIntroductionProfileResponse(
    override val sessionId: Option[String],
    override val correlationId: String)
  extends ProtocolResponseMessage {
}
