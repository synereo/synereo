// -*- mode: Scala;-*- 
// Filename:    ConcreteMessageSet.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  2 20:24:58 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.dsl._
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.msgs._

package diesel {
  object EvaluatorMessageSet extends AbstractEvaluatorMessageSet with Serializable {    
    type Alias = CnxnCtxtLabel[String,String,String]  
    type Cnxn = AgentCnxnTypes#AgentCnxn
    type Filter = CnxnCtxtLabel[String,String,String]  
    type Post = String
    type GloSExpr = ConcreteHL.HLExpr
  }
}
