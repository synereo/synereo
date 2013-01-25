package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema.Post
import reflect.BeanProperty
import scala.collection.JavaConversions._

case class InvitationResponse(
  override val ids: Identification,
  override val eventKey: EventKey,
  @BeanProperty val category: String,
  @BeanProperty val connectionType: String,
  @BeanProperty val connectionName: String,
  @BeanProperty val post: Post,
  @BeanProperty val conversationThread: java.util.List[ Post ],
  @BeanProperty val accept: Boolean,
  invitationRequest: InvitationRequest
  )
  extends Message(ids, eventKey)
  with Response
{
  if ( post != null && !post.isSent() ) post.send()

  def this() = this(null, null, null, null, null, null, null, false, null)
  // Response is created the content of the post implicitly has to be viewed
  if ( conversationThread != null )
    conversationThread.foreach(post => {if ( !post.isViewed() ) post.view()})

  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Creator)
  channelLevel = Some(ChannelLevel.Public)

  if  (invitationRequest != null){
    this.targetCnxn = invitationRequest.targetCnxn;
    this.originCnxn = invitationRequest.originCnxn;
  }
}

