package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty

/* User: jviolago
*/

case class GetClaimRequest(override val ids: Identification, override val eventKey: EventKey, claimObject:String, claimField:String, verifierList:List[Verifier], relyingAgentDescription:String) extends Message() with Request {
  override def channel = Channel.Verify
  @BeanProperty var notifyCnxn:AgentCnxn = null
}

