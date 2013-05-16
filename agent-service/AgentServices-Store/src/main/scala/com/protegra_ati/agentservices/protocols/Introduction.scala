package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class IntroductionRequest(
  override val requestId: Option[String],
  override val responseCnxn: Option[acT.AgentCnxn],
  message: Option[String])

  extends ProtocolRequestMessage {

  def this() = this(None, None, None)
}

case class IntroductionResponse(
  override val responseId: String,
  accepted: Option[Boolean],
  rejectReason: Option[String],
  connectId: Option[String])

  extends ProtocolResponseMessage {

  def this(responseId: String) = this(responseId, None, None, None)
}
