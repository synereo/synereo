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
                                  @BeanProperty val referral_A: Referral,
                                  @BeanProperty val referral_B: Referral) extends Message(ids, eventKey)
                                  with Request
{
  def this() = this(null, null, null, null) // serialization helper
  def this(
          _eventKey: EventKey,
          _referral_A: Referral,
          _referral_B: Referral) = this(new Identification(),_eventKey,_referral_A,_referral_B)

  override def channel = Channel.Referral

  if ( referral_A.post != null && !referral_A.post.isSent() ) referral_A.post.send()
  if ( referral_B.post != null && !referral_B.post.isSent() ) referral_B.post.send()

  def deliver() = {
    if ( referral_A.post != null && !referral_A.post.isDelivered() ) referral_A.post.deliver()
    if ( referral_B.post != null && !referral_B.post.isDelivered() ) referral_B.post.deliver()
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
