// -*- mode: Scala;-*- 
// Filename:    HandlerService.scala 
// Authors:     lgm                                                    
// Creation:    Wed Oct  2 17:51:11 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.trampoline
package mongodb
package record

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import java.security._


import java.util.Date
import java.util.UUID

import java.net.URI

object EvalHandlerService
extends EvaluationCommsService
with EvalConfig
with DSLCommLinkConfiguration
with AccordionConfiguration
with Serializable {
}

package usage {
  // TBD
}
