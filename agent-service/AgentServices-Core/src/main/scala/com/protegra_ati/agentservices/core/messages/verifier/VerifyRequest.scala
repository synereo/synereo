package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.messages._

/* User: jviolago
*/

case class VerifyRequest(override val ids: Identification, override val eventKey: EventKey, alias:String, claimKey:String, claimData:String, reason:String, relyingAgentDescription:String) extends Message with Request {
  @BeanProperty var relyingAgentCnxnProxy:AgentCnxnProxy = null
  override def channel = Channel.Verify
}