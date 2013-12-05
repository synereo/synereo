package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

trait ProtocolRequestMessage extends ProtocolMessage {
  val correlationId: Option[String]
  val responseCnxn: Option[PortableAgentCnxn]
}
