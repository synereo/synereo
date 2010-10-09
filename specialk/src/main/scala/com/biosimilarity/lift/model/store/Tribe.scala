// -*- mode: Scala;-*- 
// Filename:    Tribe.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct  8 17:29:51 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import org.prolog4j._

import java.net.URI

import net.liftweb.amqp._

class Tribe[Namespace,Var,Tag,Value](
  val jsonSender : StdJSONOverAMQPSender,
  val jsonDispatcher : JSONAMQPListener,
  val acquaintances : Seq[String]
) extends TermStore[Namespace,Var,Tag,Value] {
}
