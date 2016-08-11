package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class Connect(
  sessionId: String,
  correlationId: String,
  cancelled: Boolean,
  biCnxn: Option[PortableAgentBiCnxn]
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    Connect.toLabel(sessionId, correlationId)
  }
}

object Connect {
  def toLabel(sessionId: String, correlationId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(connect(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel
  }
}
