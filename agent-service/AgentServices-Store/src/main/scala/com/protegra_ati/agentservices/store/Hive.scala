// -*- mode: Scala;-*- 
// Filename:    Hive.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar  6 12:10:08 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import java.net.URI
import java.util.Date
import java.util.UUID

trait BehaviorService {
  import ConcreteBFactHL._
  type Rsrc <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteBFactHL.BFactHLExpr]#MTTypes#Resource
  type GrndRsrc <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteBFactHL.BFactHLExpr]#MTTypes#Ground
  type BndRsrcHM <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteBFactHL.BFactHLExpr]#MTTypes#RBoundHM
  type BndRsrcAList <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteBFactHL.BFactHLExpr]#MTTypes#RBoundAList

  def mapBehavior[Value](
    cnxn : Cnxn,
    label : CnxnCtxtLabel[String,String,String],
    behavior : String,
    onMap : Option[Rsrc] => Unit        
  ) : Unit
  def commenceInstance(
    behaviorDefinitionCnxn : Cnxn,
    behaviorDefinitionLabel : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    filters : Seq[CnxnCtxtLabel[String,String,String]],
    onCommencement : Option[Rsrc] => Unit
  ) : Unit
  def completeInstance(
    behaviorInstanceCnxn : Cnxn,
    behaviorInstanceLabel : CnxnCtxtLabel[String,String,String],
    onCompletion : Option[Rsrc] => Unit
  ) : Unit
}
