package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class ReceiveAMPRequestMessage(
  override val sessionId : String,
  override val correlationId : String,
  recipient: String,
  recipientKey: String,
  amount: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = ReceiveAMPRequestMessage.toLabel(sessionId)
}

object ReceiveAMPRequestMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(receiveAMPRequestMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(receiveAMPRequestMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
