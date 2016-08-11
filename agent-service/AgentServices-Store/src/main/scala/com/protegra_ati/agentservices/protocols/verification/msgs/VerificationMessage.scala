// -*- mode: Scala;-*- 
// Filename:    VerificationMessage.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 27 09:43:48 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols.msgs

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

abstract class VerificationMessage(
  override val sessionId : String,
  override val correlationId : String
) extends ProtocolMessage with SessionMsgStr
