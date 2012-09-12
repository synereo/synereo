package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.messages._

/* User: jviolago
*/

case class VerifyPermissionResponse(override val ids: Identification, isPermissionGranted:Boolean) extends Message(ids, null)
with Response
{
  override def channel = Channel.Verify

  override def toString = {
        "verifyPermissionResponse - isPermissionGranted: " + isPermissionGranted.toString
  }
}

