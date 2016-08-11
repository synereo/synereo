package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentBiCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class ConnectNotification(
  sessionId: String,
  biCnxn: PortableAgentBiCnxn,
  profileData: String
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    ConnectNotification.toLabel(sessionId)
  }
}

object ConnectNotification {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(connectNotification(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(connectNotification(sessionId(\"$sessionId\")))""".toLabel
  }
}
