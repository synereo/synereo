// -*- mode: Scala;-*- 
// Filename:    HashedConnections.scala 
// Authors:     yuvadm                                                    
// Creation:    Tue Feb 11 11:06:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

case class HashedConnections(
  override val sessionId : String,
  override val correlationId : String,
  connections: Seq[PortableAgentCnxn]
) extends ReputationMessage( sessionId, correlationId ) {
  override def toLabel : CnxnCtxtLabel[String, String, String] = {
    HashedConnections.toLabel( sessionId )
  }
}

object HashedConnections {
  def toLabel(): CnxnCtxtLabel[String, String, String] = {
    "protocolMessage(hashedConnections(sessionId(_)))".toLabel
  }

  def toLabel(sessionId: String): CnxnCtxtLabel[String, String, String] = {
    s"""protocolMessage(hashedConnections(sessionId(\"$sessionId\")))""".toLabel
  }
}
