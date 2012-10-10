/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID
import com.protegra_ati.agentservices.core.events._

case class SetContentResponse(override val ids: Identification, override val eventKey: EventKey, data: Data) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Content

  def this() = this(null, null, null)

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new SetContentResponseReceivedEvent(this)
  }
}
