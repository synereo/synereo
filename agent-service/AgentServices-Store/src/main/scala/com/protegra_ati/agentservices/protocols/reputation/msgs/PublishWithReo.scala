// -*- mode: Scala;-*- 
// Filename:    PublishWithReo.scala 
// Authors:     lgm                                                    
// Creation:    Tue Feb 11 11:06:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class PublishWithReo(
  override val sessionId : String,
  override val correlationId : String,
  publisher : PortableAgentCnxn,  
  consumer : PortableAgentCnxn,  
  content : CnxnCtxtLabel[String,String,String]
) extends ReputationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String,String,String] = {
    PublishWithReo.toLabel( sessionId )
  }
}

object PublishWithReo {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(publishWithReo(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(publishWithReo(sessionId(\"$sessionId\")))""".toLabel
  }
}
