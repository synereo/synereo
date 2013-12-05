package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class IntroductionResponse(
  sessionId: String,
  correlationId: String,
  accepted: Boolean,
  connectCorrelationId: Option[String]
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    IntroductionResponse.toLabel(sessionId, correlationId)
  }
}

object IntroductionResponse {
  def toLabel(sessionId: String, correlationId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(introductionResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel
  }
}
