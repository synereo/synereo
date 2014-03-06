package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class IntroductionConfirmation(
  sessionId: String,
  correlationId: String,
  accepted: Boolean
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    IntroductionConfirmation.toLabel(sessionId, correlationId)
  }
}

object IntroductionConfirmation {
  def toLabel(sessionId: String, correlationId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(introductionConfirmation(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel
  }
}
