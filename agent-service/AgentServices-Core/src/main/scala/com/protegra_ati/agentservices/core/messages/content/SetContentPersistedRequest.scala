package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._

case class SetContentPersistedRequest(override val eventKey: EventKey, persistedRequestData: CompositeData[ PersistedRequest ], response: Message, approved: Boolean) extends Message(new Identification(), eventKey) //extends Message(eventKey)  kryo workaround
with Request
{
  def this() = this(null, null, null, false)

  override def channel = Channel.Content
}