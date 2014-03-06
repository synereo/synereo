// -*- mode: Scala;-*- 
// Filename:    VerificationNotification.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 13 05:45:58 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class VerificationNotification(
  override val sessionId : String,
  override val correlationId : String,
  claimant : PortableAgentCnxn,
  claim : CnxnCtxtLabel[String,String,String],
  witness : CnxnCtxtLabel[String,String,String]
) extends VerificationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    VerificationNotification.toLabel( sessionId )
  }
}

object VerificationNotification {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(verificationNotification(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(verificationNotification(sessionId(\"$sessionId\")))""".toLabel
  }
}
