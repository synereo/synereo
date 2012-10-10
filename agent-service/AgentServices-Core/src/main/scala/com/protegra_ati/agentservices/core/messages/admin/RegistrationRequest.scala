package com.protegra_ati.agentservices.core.messages.admin

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID

case class RegistrationRequest(override val eventKey: EventKey, appId: UUID, agentName: String) extends Message(new Identification(),eventKey) with //Message(eventKey) with  -- kryo workaround
Request
{
  def this() = this(null, null, null)
  override def channel = Channel.Admin
}