package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class GetIntroductionProfileRequest(
  override val requestId: Option[String],
  override val responseCnxn: Option[acT.AgentCnxn])

  extends ProtocolRequestMessage {

  def this() = this(None, None)
}

// TODO: Add introduction profile to message
case class GetIntroductionProfileResponse(override val responseId: String) extends ProtocolResponseMessage {
}
