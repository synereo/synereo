package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn

case class IntroductionNotification(
    override val sessionId: Option[String],
    correlationId: String,
    initiatorBiCnxn: PortableAgentBiCnxn,
    message: Option[String],
    profileData: String)
  extends ProtocolMessage
