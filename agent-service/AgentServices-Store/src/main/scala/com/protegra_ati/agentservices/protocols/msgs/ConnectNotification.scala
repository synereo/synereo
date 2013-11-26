package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn

case class ConnectNotification(
    override val sessionId: Option[String],
    biCnxn: PortableAgentBiCnxn,
    profileData: String)
  extends ProtocolMessage