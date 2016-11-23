package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class ReceiveBTCRequestMessage(
 override val sessionId : String,
 override val correlationId : String,
 recipient: String,
 recipientKey: String,
 amount: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = ReceiveBTCRequestMessage.toLabel(sessionId)
}

object ReceiveBTCRequestMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(sendBtcsRequestMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(sendBtcsRequestMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
