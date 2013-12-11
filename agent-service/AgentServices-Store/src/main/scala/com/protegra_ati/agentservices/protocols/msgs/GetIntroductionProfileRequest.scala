package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class GetIntroductionProfileRequest(
  sessionId: String,
  correlationId: String,
  responseCnxn: PortableAgentCnxn
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    GetIntroductionProfileRequest.toLabel(sessionId)
  }
}

object GetIntroductionProfileRequest {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(getIntroductionProfileRequest(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(getIntroductionProfileRequest(sessionId(\"$sessionId\")))""".toLabel
  }
}
