// -*- mode: Scala;-*- 
// Filename:    CompleteClaim.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 13 04:37:28 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class CompleteClaim(
  override val sessionId : String,
  override val correlationId : String,
  verifier : PortableAgentCnxn,
  claim : CnxnCtxtLabel[String,String,String],
  data : CnxnCtxtLabel[String,String,String]
) extends VerificationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    CompleteClaim.toLabel( sessionId )
  }
}

object CompleteClaim {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(completeClaim(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(completeClaim(sessionId(\"$sessionId\")))""".toLabel
  }
}

