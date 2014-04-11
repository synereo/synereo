// -*- mode: Scala;-*- 
// Filename:    BTCHandler.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 10 15:49:40 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution.portable.btc.v0_1._

import com.protegra_ati.agentservices.store._
import com.protegra_ati.agentservices.protocols.msgs._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.evaluator.msgs.agent.crud._
import com.biosimilarity.evaluator.prolog.PrologDSL._
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

trait BTCHandlerSchema {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT
  import ConcreteHL._
}

trait BTCHandler extends BTCHandlerSchema {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT
  import ConcreteHL._
  
  def handleSupportRequest(
    msg : supportRequest
  ) : Unit = {
    // set up a handler for the callback on payment receipt
    // create a receiving address for the recipient    
    // issue payment from the supporter
  }
  
  def handleReceivingAddressResponse(
    msg : receivingAddressResponse
  ) : Unit = {
  }

  def handlePaymentNotification(
    msg : receivingAddressResponse
  ) : Unit = {
  }
}
