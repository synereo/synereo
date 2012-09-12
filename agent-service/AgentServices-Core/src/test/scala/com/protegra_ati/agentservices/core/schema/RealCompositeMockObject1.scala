package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.messages.EventKey
import reflect.BeanProperty
import org.joda.time.DateTime

/* User: mgevantmakher
*/

case class RealCompositeMockObject1(val eventKey: EventKey, val eventKey1: EventKey,val invitationConnectionId: String,
  @BeanProperty var selfAlias: String, val created: DateTime)
{
  def this() = this(null, null,"", "", null)
}