package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.events._

/* User: jviolago
*/

case class VerifyResponse(override val ids: Identification, override val eventKey: EventKey, alias:String, claimKey:String, isVerified:Boolean) extends Message(ids, null)
with Response
with EventProducer[Response]
{
  def this() = this(null, null, null, null, false)

  override def channel = Channel.Verify

  def generateEvent(): MessageEvent[ _ <: Message with Response ] =
  {
    new VerifyResponseReceivedEvent(this)
  }
}