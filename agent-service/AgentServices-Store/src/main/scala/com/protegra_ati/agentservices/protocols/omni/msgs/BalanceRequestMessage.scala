package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._


case class BalanceRequestMessage(
  override val sessionId : String,
  override val correlationId : String,
  user: String,
  privKey: String
) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = BalanceRequestMessage.toLabel(sessionId)
}

object BalanceRequestMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(balanceRequestMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(balanceRequestMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}

