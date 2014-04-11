// -*- mode: Scala;-*- 
// Filename:    BTCMsgs.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 10 15:56:59 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution.portable.btc.v0_1

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.dsl._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

trait BTCMessage

trait UserRequest
trait BlockChainResponse
trait BlockChainCB

case class supportRequest(
  splix : Double,
  from : PortableAgentCnxn,
  to : PortableAgentCnxn
) extends BTCMessage with UserRequest

case class receivingAddressResponse(
  inputAddress : String,
  callbackURL : String,
  feePercent : Int,
  destination : String
) extends BTCMessage with BlockChainResponse

case class paymentNotification(
  callbackURL : String
) extends BTCMessage with BlockChainCB
