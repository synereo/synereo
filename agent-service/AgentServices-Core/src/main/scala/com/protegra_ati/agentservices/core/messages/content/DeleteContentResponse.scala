/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import java.util.UUID
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.schema.Data

case class DeleteContentResponse(override val ids: Identification, override val eventKey: EventKey, queryObject: Data) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this() = this(null, null, null)
  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Content

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new DeleteContentResponseReceivedEvent(this)
  }
}