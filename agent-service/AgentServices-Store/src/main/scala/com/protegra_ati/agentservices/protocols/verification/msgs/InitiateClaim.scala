// -*- mode: Scala;-*- 
// Filename:    InitiateClaim.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 11 11:06:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class InitiateClaim(
  override val sessionId : String,
  override val correlationId : String,
  verifier : PortableAgentCnxn,  
  relyingParty : PortableAgentCnxn,  
  claim : CnxnCtxtLabel[String,String,String]
) extends VerificationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    InitiateClaim.toLabel( sessionId )
  }
}

object InitiateClaim {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(initiateClaim(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(initiateClaim(sessionId(\"$sessionId\")))""".toLabel
  }
}
