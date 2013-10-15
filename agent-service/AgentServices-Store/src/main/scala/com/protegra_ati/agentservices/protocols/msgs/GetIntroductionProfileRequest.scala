package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class GetIntroductionProfileRequest(
    override val sessionId: Option[String],
    override val correlationId: Option[String],
    override val responseCnxn: Option[acT.AgentCnxn])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None)
}
