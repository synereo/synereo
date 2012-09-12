package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._

/**
 * Sets Data to the self-connection of the conversation partner
 * @param ids
 * @param eventKey
 * @param oldData
 * @param newData
 */
case class SetSelfContentRequest(override val ids: Identification, override val eventKey: EventKey, newData: Data, oldData: Data) extends Message(ids,eventKey) with
Request
{
  def this () = this (null, null, null, null)
  override def channel = Channel.Content

 // override val channelRole = Some(ChannelRole.Consumer)
  channelLevel = Some(ChannelLevel.Public)
}