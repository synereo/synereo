package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class BeginIntroductionRequest(
    override val sessionId: Option[String],
    override val requestId: Option[String],
    override val responseCnxn: Option[acT.AgentCnxn],
    aRequestCnxn: Option[acT.AgentCnxn],
    aResponseCnxn: Option[acT.AgentCnxn],
    bRequestCnxn: Option[acT.AgentCnxn],
    bResponseCnxn: Option[acT.AgentCnxn],
    aMessage: Option[String],
    bMessage: Option[String])
  extends ProtocolRequestMessage {

  def this() = this(None, None, None, None, None, None, None, None, None)
}
