// -*- mode: Scala;-*- 
// Filename:    RLLServer.scala 
// Authors:     lgm                                                    
// Creation:    Sun Oct 16 18:51:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._
import SyntaxConversion._

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

import java.net.URI
import java.util.UUID
import java.io.StringReader

object RLLEvaluationService {
  import com.biosimilarity.lift.model.store.usage._
  import PersistedMonadicTS._
  
  val repl1 = new RLLREPL()
}
