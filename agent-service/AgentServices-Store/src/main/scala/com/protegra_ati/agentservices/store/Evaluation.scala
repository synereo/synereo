// -*- mode: Scala;-*- 
// Filename:    Evaluation.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jan 22 09:14:12 2014 
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

trait EvaluationService {
  import ConcreteHL._
  type Rsrc <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]#MTTypes#Resource
  type GrndRsrc <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]#MTTypes#Ground
  type BndRsrcHM <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]#MTTypes#RBoundHM
  type BndRsrcAList <: PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]#MTTypes#RBoundAList

  def post[Value](
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    content : Value,
    onPost : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def postV[Value](
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    content : Value,
    onPost : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def put[Value](
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    content : Value,
    onPut : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def read(
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    onReadRslt : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) },
    sequenceSubscription : Boolean = false
  ) : Unit
  def fetch(
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    onFetchRslt : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def feed(
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    onFeedRslt : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def get(
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    onGetRslt : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def score(
    filter : CnxnCtxtLabel[String,String,String],
    cnxns : Seq[Cnxn],
    staff : Either[Seq[Cnxn],Seq[Label]],
    onScoreRslt : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
  ) : Unit
  def cancel(
    filter : CnxnCtxtLabel[String,String,String],
    connections : Seq[Cnxn],
    onCancel : Option[Rsrc] => Unit =
      ( optRsrc : Option[Rsrc] ) => { BasicLogService.tweet( "onCancel: optRsrc = " + optRsrc ) }
  ) : Unit  
  def runProcess(
    cmd : String,
    wkDir : Option[String],
    env : Seq[( String, String )],
    onExecution : Option[Rsrc] => Unit
  ) : Unit
}
