/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.events._

//needs to be java.util.List for interop with java.
//easier for scala to include import scala.collection.JavaConversions._ to implicitly go back and forth than java to work with scala collections

case class GetContentResponse(override val ids: Identification, override val eventKey: EventKey, data: java.util.List[Data]) extends Message(ids, eventKey)
with Response
with EventProducer[Response]
{
  def this () = this(null,null,null)
  channelLevel = Some(ChannelLevel.Public)
  override def channel = Channel.Content

  var contentConnection: Option[ Connection ] = None

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new GetContentResponseReceivedEvent(this)
  }
}
