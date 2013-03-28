package com.protegra_ati.agentservices.core.schema

import behaviors._
import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.messages.{Identification, Message}
import com.protegra_ati.agentservices.core.schema.persistence._
import org.joda.time.{DateTime, Instant}
import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.store.extensions.StringExtensions._


case class PersistedMessage[ T <: Message ](@BeanProperty val message: T)
  extends Data
  with Archive
  with Ignore
  with Reject
  with View
  with Serializable
  with KVDBSerializable
  with StorablePersistedMessageDataDefaults
  with UINotifiable
{
   def this() = this(null.asInstanceOf[T])

  var messageType = formattedMessageName
  var messageId = formattedMessageId
  var messageParentId = formattedMessageParentId
  var messageConversationId = formattedMessageConversationId

  @BeanProperty
  var persisted: DateTime = null

  def formattedMessageName: String =
  {
    if (message == null)
      return ""

    message.getClass.getName.trimPackage
  }

  def formattedMessageId(): String =
  {
    if (message == null || message.ids == null)
      return null

    message.ids.id
  }


  def formattedMessageParentId(): String =
  {
    if (message == null || message.ids == null)
      return null

    message.ids.parentId
  }


  def formattedMessageConversationId(): String =
  {
    if (message == null || message.ids == null)
      return null

    message.ids.conversationId
  }


  override def hashCode = 41 * super.hashCode + ( if ( getArchived != null ) getArchived.hashCode else 0 ) + ( if ( getIgnored != null ) getIgnored.hashCode else 0 ) + ( if ( getRejected != null ) getRejected.hashCode else 0 )

  override def equals(other: Any): Boolean =
  {
//    println("CALL EQUALS ON PERSISTED MESSAGE")
     other match {

      case that: PersistedMessage[ Message ] => {
        if ( that canEqual this ) {
          if ( this.message == null && that.message == null ) return true
          else if ( this.message == null && that.message != null || this.message != null && that.message == null ) return false
          return this.message.equals(that.message) && this.getArchived == that.getArchived && this.getIgnored == that.getIgnored && this.getRejected == that.getRejected && this.getViewed == that.getViewed
        } else false
      }
    case _ =>
      false
  }
    false
  }


  override def canEqual(other: Any) =
    other.isInstanceOf[ PersistedMessage[Message] ]


}
