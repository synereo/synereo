package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class IntroductionNotification(
  sessionId: String,
  correlationId: String,
  initiatorBiCnxn: PortableAgentBiCnxn,
  message: Option[String],
  profileData: String
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    IntroductionNotification.toLabel(sessionId)
  }
}

object IntroductionNotification {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(introductionNotification(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(introductionNotification(sessionId(\"$sessionId\")))""".toLabel
  }
}
