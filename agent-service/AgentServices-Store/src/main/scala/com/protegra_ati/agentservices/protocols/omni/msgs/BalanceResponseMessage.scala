package com.protegra_ati.agentservices.protocols.omni.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._


case class BalanceResponseMessage(
    override val sessionId : String,
    override val correlationId : String,
    address: String,
    amp: String,
    btc: String,
    errors: String
  ) extends OmniMessage(sessionId, correlationId) {
  override def toLabel: CnxnCtxtLabel[String, String, String] = BalanceResponseMessage.toLabel(sessionId)
}

object BalanceResponseMessage {
  def toLabel: CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(balanceResponseMessage(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(balanceResponseMessage(sessionId(\"$sessionId\")))""".toLabel
  }
}
