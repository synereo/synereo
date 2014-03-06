package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import java.util.UUID

trait ProtocolMessage {
  def toLabel : CnxnCtxtLabel[String, String, String]
}

trait SessionMessageP[IDType] {
  def sessionId : IDType
  def correlationId : IDType
}

trait SessionMsgStr extends SessionMessageP[String]
trait SessionMsgUUID extends SessionMessageP[UUID]
