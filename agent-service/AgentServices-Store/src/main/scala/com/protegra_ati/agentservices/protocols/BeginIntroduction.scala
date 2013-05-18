package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class BeginIntroductionRequest(
  override val requestId: Option[String],
  override val responseCnxn: Option[acT.AgentCnxn],
  aRequestCnxn: Option[acT.AgentCnxn],
  aResponseCnxn: Option[acT.AgentCnxn],
  bRequestCnxn: Option[acT.AgentCnxn],
  bResponseCnxn: Option[acT.AgentCnxn],
  aMessage: Option[String],
  bMessage: Option[String])

  extends ProtocolRequestMessage {

  def this() = this(None, None, None, None, None, None, None, None)
}

case class BeginIntroductionResponse(
  override val responseId: String,
  accepted: Option[Boolean],
  aRejectReason: Option[String],
  bRejectReason: Option[String])

  extends ProtocolResponseMessage {

  def this(responseId: String) = this(responseId, None, None, None)
}
