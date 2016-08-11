package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class GetIntroductionProfileResponse(
  sessionId: String,
  correlationId: String,
  profileData: String
) extends ProtocolMessage {
  override def toLabel: CnxnCtxtLabel[String, String, String] = {
    GetIntroductionProfileResponse.toLabel(sessionId, correlationId)
  }
}

object GetIntroductionProfileResponse {
  def toLabel(sessionId: String, correlationId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(getIntroductionProfileResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel
  }
}
