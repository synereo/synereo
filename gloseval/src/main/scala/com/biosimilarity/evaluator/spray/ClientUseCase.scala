// -*- mode: Scala;-*- 
// Filename:    ClientUseCase.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 15 14:19:36 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import akka.actor._
import spray.routing._
import directives.CompletionMagnet
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._

import spray.httpx.encoding._

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec

import java.util.Date
import java.util.UUID

package usage {
  object SimpleClient
    extends EvaluationCommsService  
     with EvalHandler
     with MessageGeneration
     with ChannelGeneration
     with EvalConfig
     with DSLCommLinkConfiguration
     with Serializable
  {
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  }
}
