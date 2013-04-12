package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema.Post
import reflect.BeanProperty
import scala.collection.JavaConversions._

case class ReferralResponse(
  override val ids: Identification,
  override val eventKey: EventKey,
  @BeanProperty val source: CreateInvitationRequest,
  @BeanProperty val postToTarget: Post,
  @BeanProperty val postToSource: Post,
  @BeanProperty val accept: Boolean
  )
  extends Message(ids, eventKey)
  with Response
{
  def this() = this(null, null, null, null, null, false)

  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Creator)
  channelLevel = Some(ChannelLevel.Public)

  if ( postToTarget != null && !postToTarget.isSent() ) postToTarget.send()
  if ( postToSource != null && !postToSource.isSent() ) postToSource.send()

  def deliver() = {
    if ( postToTarget != null && !postToTarget.isDelivered() ) postToTarget.deliver()
    if ( postToSource != null && !postToSource.isDelivered() ) postToSource.deliver()
  }
}