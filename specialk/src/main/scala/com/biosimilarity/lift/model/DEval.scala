// -*- mode: Scala;-*- 
// Filename:    DEval.scala 
// Authors:     lgm                                                    
// Creation:    Wed Oct 26 05:40:56 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import java.net.URI
import java.util.UUID

trait DEvalMessages {
  trait evaluationProtocolMsg
  trait request
  trait response
  trait sessionSetup extends evaluationProtocolMsg
  case class sessionRequest( id : String )
       extends sessionSetup with request
  case class sessionResponse( id : String )
       extends sessionSetup with response
  
  trait evaluation extends evaluationProtocolMsg
  case class evalExprRequest(
    sessId : String,
    flowId : String, // useful to make this hash of expr
    expr : String    
  ) extends evaluation with request
  case class evalExprResponse(
    sessId : String,
    flowId : String, // useful to make this hash of expr
    rslt : String
  ) extends evaluation with response

  def protoSessionRequest : sessionRequest
  lazy val theProtoSessionRequest : sessionRequest =
    protoSessionRequest
  def protoSessionResponse : sessionResponse
  lazy val theProtoSessionResponse : sessionResponse =
    protoSessionResponse
  def protoEvaluationRequest : evalExprRequest
  lazy val theProtoEvaluationRequest : evalExprRequest =
    protoEvaluationRequest
  def protoEvaluationResponse : evalExprResponse
  lazy val theProtoEvaluationResponse : evalExprResponse =
    protoEvaluationResponse
}

trait DEvalMsgScope {
  type EvalMsgTypes <: DEvalMessages
  def protoEvalMsgs : EvalMsgTypes
  val EvalMsgSet : EvalMsgTypes = protoEvalMsgs
}
