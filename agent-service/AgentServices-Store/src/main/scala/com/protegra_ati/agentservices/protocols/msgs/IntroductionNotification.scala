package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

case class IntroductionNotification(
    override val sessionId: Option[String],
    correlationId: String,
    initiatorBiCnxn: acT.AgentBiCnxn,
    message: Option[String])
  extends ProtocolMessage
