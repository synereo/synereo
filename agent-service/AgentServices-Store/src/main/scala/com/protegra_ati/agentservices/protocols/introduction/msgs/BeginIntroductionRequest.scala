package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class BeginIntroductionRequest(
  sessionId: String,
  aBiCnxn: PortableAgentBiCnxn,
  bBiCnxn: PortableAgentBiCnxn,
  aMessage: Option[String],
  bMessage: Option[String]
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    BeginIntroductionRequest.toLabel(sessionId)
  }
}

object BeginIntroductionRequest {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(beginIntroductionRequest(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(beginIntroductionRequest(sessionId(\"$sessionId\")))""".toLabel
  }
}
