package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class SendAmpsResponseMessage(
 override val sessionId : String,
 override val correlationId : String,
 sender: String,
 recipient: String,
 transaction: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = SendAmpsResponseMessage.toLabel(sessionId)
}

object SendAmpsResponseMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(sendAmpsResponseMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(sendAmpsResponseMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
