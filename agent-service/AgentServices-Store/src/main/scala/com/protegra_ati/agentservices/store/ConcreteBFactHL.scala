// -*- mode: Scala;-*- 
// Filename:    ConcreteHL.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr 27 02:42:41 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.dsl.bfact._
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

import java.net.URI

object ConcreteBFactHL extends AbstractBFactHL with Serializable {  
  type Label = CnxnCtxtLabel[String,String,String]  
  type Substitution = Option[List[(String,Label)]]  

  type Cnxn = PortableAgentCnxn
  type BiCnxn = PortableAgentBiCnxn
}
