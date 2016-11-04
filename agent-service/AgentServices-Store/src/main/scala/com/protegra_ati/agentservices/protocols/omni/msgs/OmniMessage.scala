package com.protegra_ati.agentservices.protocols.omni.msgs

import com.protegra_ati.agentservices.protocols.msgs.{ProtocolMessage, SessionMsgStr}

abstract class OmniMessage(
  override val sessionId : String,
  override val correlationId : String
) extends ProtocolMessage with SessionMsgStr

