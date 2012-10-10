package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.events._

case class VerifyContentResponse(override val ids: Identification, override val eventKey: EventKey, contentVerifier:ContentVerifier, value:String, claimingAgentCnxnProxy:AgentCnxnProxy, isApproved:Boolean) extends Message(ids, null)
with Response
with EventProducer[Response]
{
  override def channel = Channel.Verify

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new VerifyContentResponseReceivedEvent(this)
  }
}

