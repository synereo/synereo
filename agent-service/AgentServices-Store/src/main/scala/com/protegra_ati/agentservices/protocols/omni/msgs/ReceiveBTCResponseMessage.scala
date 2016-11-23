package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class ReceiveBTCResponseMessage(
 override val sessionId : String,
 override val correlationId : String,
 recipient: String,
 transaction: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = ReceiveBTCResponseMessage.toLabel(sessionId)
}

object ReceiveBTCResponseMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(receiveBTCResponseMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(receiveBTCResponseMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
