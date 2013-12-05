package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

case class IntroductionRequest(
    override val sessionId: Option[String],
    override val correlationId: Option[String],
    override val responseCnxn: Option[PortableAgentCnxn],
    message: Option[String],
    profileData: Option[String])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None, None, None)
}
