package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.messages._
import reflect.BeanProperty
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class MockOptionObject( ) extends UseKryoSerialization
{
  
  //  val channelTestRole: Option[ String ] = None //channelRole describes the cnxn who should be listening for the request
  //
  val someTest: Option[ String ] = Some("hallo world") //channelRole describes the cnxn who should be listening for the request
  //  val channelLevel0: Option[ ChannelLevel.Value ] = None
  //  var channelLevel1: Option[ ChannelLevel.Value ] = Some(ChannelLevel.Private)
  //  //var channelRole: Option[ ChannelRole.Value ] = None //channelRole describes the cnxn who should be listening for the request
  //
  //  override def channelType: ChannelType.Value = null
  //
  //  override def channel: Channel.Value = Channel.Invitation
  //
  //  //

 // override def str = "YES"

  //toBeOverridden = ChannelRole.Creator
  val temp: Option[ String ] = Some("temp")//ChannelRole.Creator)

  //str1 = "yes"
}