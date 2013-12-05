package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn

case class BeginIntroductionRequest(
    override val sessionId: Option[String],
    aBiCnxn: Option[PortableAgentBiCnxn],
    bBiCnxn: Option[PortableAgentBiCnxn],
    aMessage: Option[String],
    bMessage: Option[String])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None, None, None)

  override val correlationId = None
  override val responseCnxn = None
}
