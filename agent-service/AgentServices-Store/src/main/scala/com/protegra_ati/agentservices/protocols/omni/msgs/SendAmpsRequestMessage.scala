package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class SendAmpsRequestMessage(
  override val sessionId : String,
  override val correlationId : String,
  sender: String,
  senderKey: String,
  recipient: String,
  recipientKey: String,
  amount: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = SendAmpsRequestMessage.toLabel(sessionId)
}

object SendAmpsRequestMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(sendAmpsRequestMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(sendAmpsRequestMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
