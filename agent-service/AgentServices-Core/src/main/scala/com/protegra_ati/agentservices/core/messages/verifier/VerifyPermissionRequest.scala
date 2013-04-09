package com.protegra_ati.agentservices.core.messages.verifier

import reflect.BeanProperty
import java.util.UUID
import com.protegra_ati.agentservices.core.messages._

/* User: jviolago
*/

case class VerifyPermissionRequest(override val ids: Identification, override val eventKey: EventKey, @BeanProperty claimKey:String, @BeanProperty claimData:String, @BeanProperty reason:String, @BeanProperty relyingAgentDescription:String) extends Message
with Request
with NotificationProducer
{
  def this() = this(null, null, null, null, null, null)

  override def channel = Channel.Verify
  channelLevel = Some(ChannelLevel.Public)

  override def toString = {
        "verifyPermissionRequest: " + claimKey + " : " + claimData + " : " + reason + " : " + relyingAgentDescription
  }

  override def generateNotification(key:EventKey): Message with Notification = {
    new VerifyPermissionRequiredNotification(key)
  }
}

