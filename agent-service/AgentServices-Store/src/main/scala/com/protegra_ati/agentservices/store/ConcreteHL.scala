// -*- mode: Scala;-*- 
// Filename:    ConcreteHL.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr 27 02:42:41 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.dsl._
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

object ConcreteHL extends AbstractHL {
  type Label = CnxnCtxtLabel[String,String,String]  
  type Cnxn = AgentCnxnTypes#AgentCnxn
  type Substitution = Option[List[(String,Label)]]
}
