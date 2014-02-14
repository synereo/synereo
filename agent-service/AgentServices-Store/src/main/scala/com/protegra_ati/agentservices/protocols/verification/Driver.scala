// -*- mode: Scala;-*- 
// Filename:    Driver.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 13 16:40:06 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.{PortableAgentCnxn, PortableAgentBiCnxn}
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.protegra_ati.agentservices.protocols.msgs._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import java.util.UUID

package usage {
  import com.biosimilarity.evaluator.distribution.NodeStreams
  import com.biosimilarity.evaluator.distribution.FuzzyStreams
  object VerificationDriver extends NodeStreams with FuzzyStreams {
  }
}
