package com.protegra_ati.agentservices.core.messages

import java.io.Serializable
import java.util.UUID
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import reflect.BeanProperty
import java.lang.reflect._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.util.serializer.{UseJavaIOSerialization}
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}


abstract class Message(val ids: Identification, val eventKey: EventKey)
  extends Serializable
  with KVDBSerializable
  with UseJavaIOSerialization
with Reporting
{
  //using null instead of none for java interop
  //  def this(ids: Identification) = this (ids, null)

  def this(eventKey: EventKey) = this(new Identification(), eventKey)

  def this() = this(new Identification(), null)

  def channel: Channel.Value //the channel used in listener
  var  channelRole: Option[ ChannelRole.Value ] = None //channelRole describes the cnxn who should be listening for the request
  def channelType: ChannelType.Value

  var channelLevel: Option[ ChannelLevel.Value ] = None
  //java shouldn't be setting this
    var originCnxn: AgentCnxnProxy = null

  def getChannelKey: String =
  {
    //showing conversationId here now to provide the ability
    //for processes to distinguish a specific stream
    //this only works if you are using the channel level
    //as most msg's without it will be gobbled up by the
    //wildcard listens
    //    if (channelLevel == None)
    //    {
    //      channelLevel = Some(ChannelLevel.Private)
    //    }

    try {
      var channelKey = channel.toString + channelRole.getOrElse("") + channelType.toString + channelLevel.getOrElse(ChannelLevel.Private).toString
      channelKey += "(\"" + ids.conversationId.toString + "\")"
      channelKey
    }
    catch {
      case e:Exception => {
        report("Error building channel key.  Channel: " + channel + "  channelRole: " + channelRole + "  channelLevel: " + channelLevel, e, Severity.Error)
        throw e
      }
    }
  }

  def getExchangeKey: String =
  {
    var channelKey = channel.toString + channelRole.getOrElse("") + channelType.toString + channelLevel.getOrElse(ChannelLevel.Private).toString
    channelKey += "(_)"
    channelKey
  }


  def isInOriginalState: Boolean =
  {
    if ( ids.conversationId.equals(ids.parentId) && ids.conversationId.equals(ids.id) )
      true
    false
  }



  //java to set or read values
  @BeanProperty var targetCnxn: AgentCnxnProxy = null
}