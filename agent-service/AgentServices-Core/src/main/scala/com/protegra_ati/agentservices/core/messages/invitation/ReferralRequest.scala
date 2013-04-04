package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import behaviors.Tracking
import reflect.BeanProperty
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class ReferralRequest(
  override val ids: Identification,
  override val eventKey: EventKey,
  @BeanProperty val source: CreateInvitationRequest
  )
  extends Message(ids, eventKey)
  with Request with UseKryoSerialization
{
  def this () = this (null, null, null)


  def getIds(): Identification =
  {
    ids
  }

  def getEventKey(): EventKey =
  {
    eventKey
  }

  override def isJavaIOSerializationDeprecated = true

  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Creator)
  channelLevel = Some(ChannelLevel.Public)

  def deliver() = {
    if (source != null)
    {
      if ( source.postToTarget != null && !source.postToTarget.isDelivered() ) source.postToTarget.deliver()
      if ( source.postToBroker != null && !source.postToBroker.isDelivered() ) source.postToBroker.deliver()
    }
  }

  override def toString: String ={
    "ReferralRequest[ ids="+ids+", eventKey="+eventKey+", source="+source+", source.origin=" + (if (source == null) "null" else source.originCnxn) + "]"
  }

//
//  override def getResponseChannelKey: String =
//  {
//    var channelKey = channel.toString + ChannelRole.Creator.toString + ChannelType.Response.toString + ChannelLevel.Single.toString
//    channelKey += "(\"" + ids.conversationId.toString + "\")"
//    channelKey
//  }
}



object ReferralRequest
{

  final val SEARCH_ALL_PERSISTED_MESSAGE_KEY = new PersistedMessage(new ReferralRequest).toSearchKey

  final val SEARCH_ALL_PERSISTED_MESSAGE = new PersistedMessage(new ReferralRequest)
  {
    override def toSearchKey(): String = ReferralRequest.SEARCH_ALL_PERSISTED_MESSAGE_KEY
  }

}
