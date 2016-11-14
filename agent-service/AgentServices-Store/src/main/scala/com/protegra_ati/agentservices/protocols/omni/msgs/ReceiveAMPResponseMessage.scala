package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class ReceiveAMPResponseMessage(
  override val sessionId : String,
  override val correlationId : String,
  recipient: String,
  transaction: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = ReceiveAMPResponseMessage.toLabel(sessionId)
}

object ReceiveAMPResponseMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(receiveAMPResponseMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(receiveAMPResponseMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
