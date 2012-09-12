package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.messages._
import reflect.BeanProperty

case class MockCreateInvitationRequest(
  @BeanProperty override val eventKey: MockEventKey,
  @BeanProperty override val eventKey2: MockEventKey,
  @BeanProperty val invitationConnectionId1: String,
  @BeanProperty val selfAlias: String,
  @BeanProperty val channelTestRole: Option[ String ],
  @BeanProperty val requestedConnectionName: Option[ String ],
  @BeanProperty val post: java.util.List[Post]

  ) extends MockMessage(eventKey, eventKey2)
{
  def this() = this(null, null, "", "", null, null, null)

  //channelRole describes the cnxn who should be listening for the request
  //
  //var someTest: Option[ String ] = Some("hallo world") //channelRole describes the cnxn who should be listening for the request
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
  // val bubu: Option[ String ] = Some("bubu")//ChannelRole.Creator)

  //str1 = "yes"
  //channelRole = Some("UHU")
}