package com.protegra_ati.agentservices.core.messages.verifier

import reflect.BeanProperty
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._

/* User: jviolago
*/

case class SelectVerifierRequest(override val ids: Identification, override val eventKey: EventKey, @BeanProperty verifierList:List[Verifier], claimObject:String, claimField:String, relyingAgentDescription:String) extends Message
with Request
with NotificationProducer
{
  override def channel = Channel.Verify

  override def generateNotification(key:EventKey): Message with Notification = {
    new SelectVerifierRequestNotification(key)
  }

}

