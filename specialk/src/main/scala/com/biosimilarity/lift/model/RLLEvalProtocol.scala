// -*- mode: Scala;-*- 
// Filename:    RLLEvalProtocol.scala 
// Authors:     lgm                                                    
// Creation:    Sun Oct 16 20:58:05 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

import java.net.URI
import java.util.UUID
import java.io.StringReader

import com.biosimilarity.lift.model.store.usage._
import PersistedMonadicTS._
import scala.util.continuations._    

trait RLLEvaluationProtocol extends CnxnXQuery[String,String,String]
 with CnxnCtxtInjector[String,String,String]
 with UUIDOps
 with Blobify
 with CnxnXML[String,String,String] {
   implicit def toPattern( s : String ) =
     fromCaseClassInstanceString( s )
     .getOrElse( null )
     .asInstanceOf[CnxnCtxtLabel[String,String,String]]

   val endPointId = getUUID
   def exchange = ptToPt( "SDEC", "localhost", "localhost" )
   val sessionRequestStr = "sessionRequest( X )"
   val sessionResponseStr =
     "sessionResponse( " + "\"" + endPointId.toString + "\"" + " )"      
   
 }

class RLLEvalProtocol(
  override val exchange : PersistedtedStringMGJ
) extends RLLEvaluationProtocol

object RLLEvalProtocol {
  def apply( exchange : PersistedtedStringMGJ ) : RLLEvalProtocol =
    new RLLEvalProtocol( exchange )
  def unapply( rllEvP : RLLEvalProtocol ) : Option[(PersistedtedStringMGJ)] = 
    Some( (rllEvP.exchange) )
}
