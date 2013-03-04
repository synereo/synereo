package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import java.util._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class CreateInvitationRequest(
  override val eventKey: EventKey,
  @BeanProperty val brokerTargetCnxnKey: String,
  @BeanProperty selfAlias: String,
  @BeanProperty targetAlias: String,
  @BeanProperty selfCategory: String,
  @BeanProperty targetCategory: String,
  @BeanProperty requestedConnectionType: String,
  @BeanProperty requestedConnectionName: String,
  @BeanProperty postToTarget: Post,
  @BeanProperty postToBroker: Post,
  @BeanProperty isRoleBasedRequest : Boolean
  ) extends Message(new Identification(), eventKey)//extends Message(eventKey) kryo workaround
   with Request with UseKryoSerialization
{
  def this () = this (null,null, null, null, null, null, null, null, null, null, false)

  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Creator)

  if ( postToTarget != null && !postToTarget.isSent() ) postToTarget.send()
  if ( postToBroker != null && !postToBroker.isSent() ) postToBroker.send()

  def deliver() = {
    if ( postToTarget != null && !postToTarget.isDelivered() ) postToTarget.deliver()
    if ( postToBroker != null && !postToBroker.isDelivered() ) postToBroker.deliver()
  }
  override def isJavaIOSerializationDeprecated: Boolean = true

}