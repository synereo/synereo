package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.events._

/* User: jviolago
*/

case class GetClaimResponse(override val ids: Identification, override val eventKey: EventKey, claimObject:String, claimField:String, verifier:Verifier) extends Message(ids, null)
with Response
with EventProducer[Response]
{
  override def channel = Channel.Verify

  var claimValue:String = ""
  var alias:String = ""

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new GetClaimResponseReceivedEvent(this)
  }
}

