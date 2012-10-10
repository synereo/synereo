/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._

case class DeleteContentRequest (override val eventKey: EventKey, queryObject: Data) extends Message(new Identification(), eventKey) //extends Message(eventKey) -- kryo workaround
with Request
{
  def this() = this(null, null)
  override def channel = Channel.Content
}