// -*- mode: Scala;-*- 
// Filename:    IntersectionResult.scala 
// Authors:     yuvadm                                                    
// Creation:    Tue Feb 11 11:06:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class IntersectionResult(
  override val sessionId : String,
  override val correlationId : String,
  publisher : PortableAgentCnxn,  
  consumer : PortableAgentCnxn,  
  content : CnxnCtxtLabel[String]
) extends ReputationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String] = {
    IntersectionResult.toLabel( sessionId )
  }
}

object IntersectionResult {
  def toLabel(): CnxnCtxtLabel[String] = {
    "protocolMessage(intersectionResult(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String] = {
    s"""protocolMessage(intersectionResult(sessionId(\"$sessionId\")))""".toLabel
  }
}
