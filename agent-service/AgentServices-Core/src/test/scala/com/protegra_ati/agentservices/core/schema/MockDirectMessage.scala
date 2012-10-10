package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.messages.{ChannelLevel, EventKey, Identification, Message, Channel,Request, ChannelRole}
import reflect.BeanProperty
import scala.collection.JavaConversions._
/* User: mgevantmakher
*/

case class MockDirectMessage(
  override val ids: Identification,
  override val eventKey: EventKey,
  @BeanProperty val alias: String,
  @BeanProperty requestedCategory: Option[ String ],
  @BeanProperty testNone: Option[ String ],
  @BeanProperty val test2: Option[ String ],
  @BeanProperty val conversationThread: java.util.List[ Post ]) extends Message(ids, eventKey) with Request
{

  if ( conversationThread != null )
    conversationThread.foreach(post => {if ( !post.isDelivered() ) post.deliver()})
  channelLevel = Some(ChannelLevel.Public)
  channelRole = Some(ChannelRole.Consumer)

  def this() = this(null, null,null, null, null, null, null)

  //override def channelType = ChannelType.Request

  override def channel = Channel.Global
}


