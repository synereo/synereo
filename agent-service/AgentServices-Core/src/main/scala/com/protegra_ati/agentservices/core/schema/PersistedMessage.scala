package com.protegra_ati.agentservices.core.schema

import behaviors._
import java.io.Serializable
import com.protegra.agentservicesstore.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.messages.Message
import com.protegra_ati.agentservices.core.schema.persistence._
import org.joda.time.{DateTime, Instant}
import scala.reflect.BeanProperty


case class PersistedMessage[ T <: Message ](@BeanProperty val message: T)
  extends Data
  with Archive
  with Ignore
  with Reject
  with Serializable
  with KVDBSerializable
  with StorablePersistedMessageDataDefaults
{
   def this() = this(null.asInstanceOf[T])

  @BeanProperty
  var persisted: DateTime = null

  override def hashCode = 41 * super.hashCode + ( if ( getArchived != null ) getArchived.hashCode else 0 ) + ( if ( getIgnored != null ) getIgnored.hashCode else 0 ) + ( if ( getRejected != null ) getRejected.hashCode else 0 )

  override def equals(other: Any): Boolean =
  {
//    println("CALL EQUALS ON PERSISTED MESSAGE")
     other match {

      case that: PersistedMessage[ Message ] => {
        if ( that canEqual this ) {
          if ( this.message == null && that.message == null ) return true
          else if ( this.message == null && that.message != null || this.message != null && that.message == null ) return false
          return this.message.equals(that.message) && this.getArchived == that.getArchived && this.getIgnored == that.getIgnored && this.getRejected == that.getRejected
        } else false
      }
    case _ =>
      false
  }
    false
  }


  override def canEqual(other: Any) =
    other.isInstanceOf[ PersistedMessage[Message] ]

//
// override  def toStoreKey: String =
//  {
//    "persistedMessage(" + message + ")"
//  }
//
//  override  def toSearchKey: String =
//  {
//    "persistedMessage(" + message + ")"
//  }

}


object PersistedMessage
{

  final val SEARCH_ALL_KEY = new PersistedMessage().toSearchKey

  final val SEARCH_ALL = new PersistedMessage()
  {
    override def toSearchKey(): String = PersistedMessage.SEARCH_ALL_KEY
  }

}
