package com.protegra_ati.agentservices.core.messages

import java.io.Serializable
import java.util.UUID
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope._
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import reflect.BeanProperty
import java.lang.reflect._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

class MockMessage(@BeanProperty val ids: Identification, @BeanProperty val eventKey: MockEventKey, @BeanProperty val eventKey2: MockEventKey) extends Serializable with
KVDBSerializable  with UseKryoSerialization
{
  //using null instead of none for java interop
  //  def this(ids: Identification) = this (ids, null)
  def this(_eventKey: MockEventKey,_eventKey2: MockEventKey) = this(new Identification(), _eventKey, _eventKey2)

  //def this(_eventKey: EventKey) = this(new Identification(), _eventKey, null)

  def this() = this(new Identification(), null, null)

  def str = "NO"

  //  val channelRole: Option[ ChannelRole.Value ] = None //channelRole describes the cnxn who should be listening for the request
  //  val channelLevel: Option[ ChannelLevel.Value ] = None
  //
  //  //java shouldn't be setting this
  //  var originCnxn: AgentCnxnProxy = null
  //var str1 = "NO"
  // var channelRole: Option[ String ] = None
  //java shouldn't be setting this
  //var originCnxn: AgentCnxnProxy = null

}