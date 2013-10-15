package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class BeginIntroductionRequest(
    override val sessionId: Option[String],
    aBiCnxn: Option[acT.AgentBiCnxn],
    bBiCnxn: Option[acT.AgentBiCnxn],
    aMessage: Option[String],
    bMessage: Option[String])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None, None, None)

  override val correlationId = None
  override val responseCnxn = None
}
