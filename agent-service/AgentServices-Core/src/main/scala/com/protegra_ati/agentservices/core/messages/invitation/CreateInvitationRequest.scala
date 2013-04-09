package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import java.util._
import reflect.BeanProperty

case class CreateInvitationRequest(
  override val ids: Identification,
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
  )

  extends Message(ids, eventKey)
  with Request
{
  def this () = this (null,null,null, null, null, null, null, null, null, null, null, false)
  def this ( _eventKey: EventKey,
    _brokerTargetCnxnKey: String,
    _selfAlias: String,
    _targetAlias: String,
    _selfCategory: String,
    _targetCategory: String,
    _requestedConnectionType: String,
    _requestedConnectionName: String,
    _postToTarget: Post,
    _postToBroker: Post,
    _isRoleBasedRequest : Boolean) = this (new Identification(),_eventKey,_brokerTargetCnxnKey,_selfAlias,_targetAlias,_selfCategory,_targetCategory,_requestedConnectionType,_requestedConnectionName,_postToTarget,_postToBroker,_isRoleBasedRequest)
             
  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Creator)

  if ( postToTarget != null && !postToTarget.isSent() ) postToTarget.send()
  if ( postToBroker != null && !postToBroker.isSent() ) postToBroker.send()

  def deliver() = {
    if ( postToTarget != null && !postToTarget.isDelivered() ) postToTarget.deliver()
    if ( postToBroker != null && !postToBroker.isDelivered() ) postToBroker.deliver()
  }
}


object CreateInvitationRequest
{

  final val SEARCH_ALL_PERSISTED_MESSAGE_KEY = new PersistedMessage(new CreateInvitationRequest).toSearchKey

  final val SEARCH_ALL_PERSISTED_MESSAGE = new PersistedMessage(new CreateInvitationRequest)
  {
    override def toSearchKey(): String = CreateInvitationRequest.SEARCH_ALL_PERSISTED_MESSAGE_KEY
  }

}
