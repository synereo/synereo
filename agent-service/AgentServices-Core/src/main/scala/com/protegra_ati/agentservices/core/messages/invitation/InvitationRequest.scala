package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID
import reflect.BeanProperty
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

/**
 * Represents an invitation to a connection
 * @param ids
 * @param eventKey
 * @param alias
 * @param requestedConnectionType  corresponding to a Connection.connectionType or 'None' if not required (especially for referrals)
 */
case class InvitationRequest(
  override val ids: Identification,
  override val eventKey: EventKey,
  @BeanProperty val alias: String,
  @BeanProperty requestedCategory: Option[ String ],
  @BeanProperty val requestedConnectionType: Option[ String ],
  @BeanProperty val requestedConnectionName: Option[ String ],
  @BeanProperty val conversationThread: java.util.List[ Post ])
  extends Message(ids, eventKey) with Request with UseKryoSerialization
{
  if ( conversationThread != null )
    conversationThread.foreach(post => {if ( !post.isDelivered() ) post.deliver()})

  def this() = this(null, null, null, null, null, null, null)

  def getIds(): Identification =
  {
    ids
  }

  def getEventKey(): EventKey =
  {
    eventKey
  }

  override def channel = Channel.Invitation

  channelRole = Some(ChannelRole.Consumer)
  channelLevel = Some(ChannelLevel.Public)

  override def getResponseChannelKey: String =
  {
    var channelKey = channel.toString + ChannelRole.Creator.toString + ChannelType.Response.toString + ChannelLevel.Single.toString
    channelKey += "(\"" + ids.conversationId.toString + "\")"
    channelKey
  }

  override def isJavaIOSerializationDeprecated: Boolean = true

  override def hashCode =
  {41 + ( this.toString.hashCode() + ( this.channelRole + "" + this.channelLevel ).hashCode() )}


  override def equals(other: Any) =
  {

    if ( null == other ) false
    other match {
      case that: InvitationRequest => {
        if ( this eq that ) true // same reference
        if ( that canEqual this ) {

          if ( this.toString.equals(that.toString) &&
            ( ( this.channelRole + "" + this.channelLevel ).equals(that.channelRole + "" + that.channelLevel) ) ) true
          else false
        } else false
      }
      case _ =>
        false
    }
  }

  override def canEqual(other: Any) =
    other.isInstanceOf[ InvitationRequest ]

}