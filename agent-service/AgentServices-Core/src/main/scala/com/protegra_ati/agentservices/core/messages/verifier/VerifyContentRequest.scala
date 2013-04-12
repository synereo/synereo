package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema._

case class VerifyContentRequest(override val ids: Identification, override val eventKey: EventKey, contentVerifier:ContentVerifier, value:String, claimingAgentCnxnProxy: AgentCnxnProxy) extends Message
with Request
with NotificationProducer
{
  def this() = this(null, null, null, null, null)

  override def channel = Channel.Verify

  override def generateNotification(key:EventKey): Message with Notification = {
    new VerifyContentRequestNotification(key)
  }
}

