package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel

trait ProtocolMessage {
  def toLabel: CnxnCtxtLabel[String, String, String]
}
