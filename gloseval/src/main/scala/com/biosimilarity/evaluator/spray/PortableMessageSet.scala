// -*- mode: Scala;-*- 
// Filename:    PortableMessageSet.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jul 10 13:21:13 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution.portable.v0_1

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.dsl._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

trait EvaluatorMessage

case class createUserRequest(
  email: String,
  password: String,
  jsonBlob: String
)
case class createUserResponse(
  agentURI: AgentURI
)
case class createUserError(
  reason: String
)

  // initializeSessionRequest( sessionURI )
  // agentURI ::= agent://email/<emailAddress>?password=<pw> |
  //              agent://cap/<capAndMac>?password=<pw>
  
case class initializeSessionRequest(
  agentURI : AgentURI
) extends EvaluatorMessage

  // initializeSessionResponse(
  //   listOfAliases, defaultAlias, listOfLabels, listOfCnxns,
  //   lastActiveFilter,
  //   sessionURI
  // )
  // sessionURI ::= agent-session://[userToken@]sessionId[/subsessionId]*[?parm=value[,parm=value]*]

case class initializeSessionError(
  agentURI : AgentURI,
  reason : String
) extends EvaluatorMessage

case class initializeSessionResponse(
  sessionURI : AgentSessionURI,
  listOfAliases : Seq[CnxnCtxtLabel[String,String,String]],
  defaultAlias : CnxnCtxtLabel[String,String,String],
  listOfLabels : Seq[CnxnCtxtLabel[String,String,String]],
  listOfCnxns : Seq[AgentCnxnTypes#AgentCnxn],
  lastActiveFilter : CnxnCtxtLabel[String,String,String]    
) extends EvaluatorMessage

// closeSessionRequest( sessionURI )
// expect agent-session://[userToken@]sessionId[/subsessionId]*[?reason=string[,parm=value]]
case class closeSessionRequest(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage
  
// closeSessionResponse( sessionURI )
case class closeSessionResponse(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage

// sessionPing( sessionURI ) -- for long-polling timeouts
case class sessionPing(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage

// sessionPong( sessionURI ) -- for long-polling timeouts
// session reconnect success
case class sessionPong(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage

// 404 for session reconnect failure

// evalRequest( expression, sessionURI )
trait EvalRequest {
  def sessionURI : AgentSessionURI
  def expression : ConcreteHL.HLExpr    
}

object EvalRequest {
  def unapply(
    evalReq : EvalRequest
  ) : Option[( AgentSessionURI, ConcreteHL.HLExpr )] = {
    Some( ( evalReq.sessionURI, evalReq.expression ) )
  }
}

// The polling version of this request allows the client to pull
// responses in at a rate that it can absorb; and then simply stop
// evaluation when it is sated.
case class evalPollRequest(
  override val sessionURI : AgentSessionURI,
  override val expression : ConcreteHL.HLExpr
) extends EvaluatorMessage with EvalRequest

case class evalNextPageRequest(
  val sessionURI : AgentSessionURI,
  val nextPage : AgentSessionURI
) extends EvaluatorMessage 

// The subscription version of this request allows the server to
// push results to the client.
case class evalSubscribeRequest(
  override val sessionURI : AgentSessionURI,
  override val expression : ConcreteHL.HLExpr
) extends EvaluatorMessage with EvalRequest

// evalResponse( pageOfPosts, sessionURI )
trait EvalResponse {
  def sessionURI : AgentSessionURI
  def pageOfPosts : Seq[String]
}

object EvalResponse {
  def unapply(
    evalRsp : EvalResponse
  ) : Option[( AgentSessionURI, Seq[String] )] = {
    Some( ( evalRsp.sessionURI, evalRsp.pageOfPosts ) )
  }
}

// Evaluation results are always paginated. In the polling version
// of the conversation the client determines the rate at which pages
// are delivered and dealt with. In the subscription version the
// server does.
case class evalPollResponse(    
  sessionURI : AgentSessionURI,
  pageOfPosts : Seq[String],
  nextPage : AgentSessionURI
) extends EvaluatorMessage with EvalResponse

case class evalSubscribeResponse(    
  sessionURI : AgentSessionURI,
  pageOfPosts : Seq[String]
) extends EvaluatorMessage with EvalResponse

// evalComplete( pageOfPosts, sessionURI )
case class evalComplete(
  sessionURI : AgentSessionURI,
  pageOfPosts : Seq[String]    
) extends EvaluatorMessage

// evalError( sessionURI )
// expect agent-session://[userToken@]sessionId[/subsessionId]*[?reason=string[,parm=value]]
case class evalError(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage

// stopEvalRequest( sessionURI )
case class stopEvalRequest(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage
  
// stopEvalResponse( sessionURI )
case class stopEvalResponse(
  sessionURI : AgentSessionURI
) extends EvaluatorMessage
