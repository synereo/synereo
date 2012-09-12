package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._

case class SetContentAdminRequest(override val eventKey: EventKey, newData: Data, oldData:Data) extends Message(new Identification(), eventKey) //extends Message(eventKey) kryo workaround
with Request {
  def this() = this(null, null, null)
  override def channel = Channel.Content
}