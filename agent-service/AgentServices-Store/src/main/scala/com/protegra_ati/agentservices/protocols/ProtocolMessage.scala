package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

trait ProtocolMessage {
  val innerLabel = ""

  def toCnxnCtxtLabel: CnxnCtxtLabel[String, String, String] = {
    ("protocolMessage(" + getClass.getName.trimPackage.toCamelCase + "(" + innerLabel + "))").toLabel
  }
}

trait ProtocolRequestMessage extends ProtocolMessage {
  val requestId: Option[String]
  val responseCnxn: Option[acT.AgentCnxn]
}

trait ProtocolResponseMessage extends ProtocolMessage {
  val responseId: String

  override val innerLabel = "responseId(\"" + responseId + "\")"
}
