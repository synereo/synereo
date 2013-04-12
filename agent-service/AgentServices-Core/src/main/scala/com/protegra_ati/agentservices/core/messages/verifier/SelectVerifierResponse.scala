package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._

/* User: jviolago
*/

case class SelectVerifierResponse(override val ids: Identification, override val eventKey:EventKey, claimObject:String, claimField:String, verifier:Verifier) extends Message(ids, null)
with Response
{
  def this() = this(null, null, null, null, null)

  override def channel = Channel.Verify
}

