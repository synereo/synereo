package com.ati.iaservices.messages.referral

import com.protegra_ati.agentservices.core.messages.Identification
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.schema.Post
import com.protegra_ati.agentservices.core.messages.Message
import com.protegra_ati.agentservices.core.messages.Request
import com.protegra_ati.agentservices.core.messages.Channel
import com.protegra_ati.agentservices.core.schema.PersistedMessage
import scala.reflect.BeanProperty

case class CreateReferralRequest(
                                  override val ids: Identification,
                                  override val eventKey: EventKey,
                                  @BeanProperty val invitationConnectionId_A: String,
                                  @BeanProperty val invitationConnectionId_B: String,
                                  @BeanProperty val alias_A: String,
                                  @BeanProperty val alias_B: String,
                                  @BeanProperty val post_A: Post,
                                  @BeanProperty val post_B: Post) extends Message(ids, eventKey)
                                                                  with Request
{
  def this() = this(null, null, null, null, null, null, null, null) // serialization helper
  def this(
          _eventKey: EventKey,
          _invitationConnectionId_A: String,
          _invitationConnectionId_B: String,
          _alias_A: String,
          _alias_B: String,
          _post_A: Post,
          _post_B: Post) = this(new Identification(),_eventKey,_invitationConnectionId_A,_invitationConnectionId_B,_alias_A,_alias_B,_post_A,_post_B)

  override def channel = Channel.Referral

  if ( post_A != null && !post_A.isSent() ) post_A.send()
  if ( post_B != null && !post_B.isSent() ) post_B.send()

  def deliver() = {
    if ( post_A != null && !post_A.isDelivered() ) post_A.deliver()
    if ( post_B != null && !post_B.isDelivered() ) post_B.deliver()
  }
}


object CreateReferralRequest
{

  final val SEARCH_ALL_PERSISTED_MESSAGE_KEY = new PersistedMessage(new CreateReferralRequest).toSearchKey

  final val SEARCH_ALL_PERSISTED_MESSAGE = new PersistedMessage(new CreateReferralRequest)
  {
    override def toSearchKey(): String = CreateReferralRequest.SEARCH_ALL_PERSISTED_MESSAGE_KEY
  }

}
