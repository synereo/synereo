package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.events._

case class VerifyContentResponse(override val ids: Identification, override val eventKey: EventKey, contentVerifier:ContentVerifier, value:String, claimingAgentCnxnProxy:AgentCnxnProxy, isApproved:Boolean) extends Message(ids, null)
with Response
with EventProducer[Response]
{
  def this() = this(null, null, null, null, null, false)

  override def channel = Channel.Verify

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new VerifyContentResponseReceivedEvent(this)
  }
}

