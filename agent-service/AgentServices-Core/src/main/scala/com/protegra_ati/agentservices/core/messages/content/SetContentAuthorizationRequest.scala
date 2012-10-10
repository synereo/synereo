package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._

case class SetContentAuthorizationRequest( override val eventKey: EventKey, authorizationReqForConnection:CompositeData[AuthorizationRequest])extends Message(new Identification(), eventKey) // extends Message(eventKey)
with Request {
  def this() = this(null, null)
  override def channel = Channel.Content
}