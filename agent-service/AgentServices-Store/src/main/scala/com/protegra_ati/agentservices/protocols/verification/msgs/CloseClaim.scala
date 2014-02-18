// -*- mode: Scala;-*- 
// Filename:    CloseClaim.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 27 10:25:57 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class CloseClaim(
  override val sessionId : String,
  override val correlationId : String,
  verifier : PortableAgentCnxn,
  claim : CnxnCtxtLabel[String,String,String],
  data : CnxnCtxtLabel[String,String,String]
) extends VerificationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    CloseClaim.toLabel( sessionId )
  }
}

object CloseClaim {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(closeClaim(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(closeClaim(sessionId(\"$sessionId\")))""".toLabel
  }
}

