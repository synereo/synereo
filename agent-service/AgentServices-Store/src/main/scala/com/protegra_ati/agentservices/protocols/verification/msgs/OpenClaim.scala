// -*- mode: Scala;-*- 
// Filename:    OpenClaim.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 27 10:08:30 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class OpenClaim(
  override val sessionId : String,
  override val correlationId : String,
  claimant : PortableAgentCnxn,
  claim : CnxnCtxtLabel[String,String,String]
) extends VerificationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    OpenClaim.toLabel( sessionId )
  }
}

object OpenClaim {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(openClaim(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(openClaim(sessionId(\"$sessionId\")))""".toLabel
  }
}
