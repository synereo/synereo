package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class IntroductionRequest(
  sessionId: String,
  correlationId: String,
  responseCnxn: PortableAgentCnxn,
  message: Option[String],
  profileData: String
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    IntroductionRequest.toLabel(sessionId)
  }
}

object IntroductionRequest {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(introductionRequest(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(introductionRequest(sessionId(\"$sessionId\")))""".toLabel
  }
}
