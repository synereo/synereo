package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

case class GetIntroductionProfileRequest(
    override val sessionId: Option[String],
    override val correlationId: Option[String],
    override val responseCnxn: Option[PortableAgentCnxn])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None)
}
