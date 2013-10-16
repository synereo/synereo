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

import java.net.URI

case class PortableAgentCnxn(
  val src : URI,
  val label : String,
  val trgt : URI
) //extends Cnxn[URI,String,URI]

case class PortableAgentBiCnxn( readCnxn : PortableAgentCnxn, writeCnxn : PortableAgentCnxn )
  
object ConcreteHL extends AbstractHL with Serializable {  
  type Label = CnxnCtxtLabel[String,String,String]  
  type Substitution = Option[List[(String,Label)]]  

  type Cnxn = PortableAgentCnxn
  type BiCnxn = PortableAgentBiCnxn
}
