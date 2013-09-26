package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._

trait ProtocolRequestMessage extends ProtocolMessage {
  val requestId: Option[String]
  val responseCnxn: Option[acT.AgentCnxn]
}
