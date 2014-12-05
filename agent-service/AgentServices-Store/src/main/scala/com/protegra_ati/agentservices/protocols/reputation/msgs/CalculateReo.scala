// -*- mode: Scala;-*- 
// Filename:    CalculateReo.scala 
// Authors:     yuvadm                                                    
// Creation:    Tue Feb 11 11:06:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class CalculateReo(
  override val sessionId : String,
  override val correlationId : String,
  publisher : PortableAgentCnxn,  
  consumer : PortableAgentCnxn,  
  content : CnxnCtxtLabel[String, String]
) extends ReputationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String, String] = {
    CalculateReo.toLabel( sessionId )
  }
}

object CalculateReo {
  def toLabel(): CnxnCtxtLabel[String, String] = {
    "protocolMessage(calculateReo(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String] = {
    s"""protocolMessage(calculateReo(sessionId(\"$sessionId\")))""".toLabel
  }
}
