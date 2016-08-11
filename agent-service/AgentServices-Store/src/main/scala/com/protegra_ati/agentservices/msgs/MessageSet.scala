// -*- mode: Scala;-*- 
// Filename:    MessageSet.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 20 17:35:14 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.msgs

import java.util.UUID
import java.net.URI

trait AbstractEvaluatorMessageSet {
  type Alias
  type Cnxn
  type Filter
  type Post
  type GloSExpr

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
    listOfAliases : Seq[Alias],
    defaultAlias : Alias,
    listOfLabels : Seq[Alias],
    listOfCnxns : Seq[Cnxn],
    lastActiveFilter : Filter    
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
    def expression : GloSExpr    
  }

  object EvalRequest {
    def unapply(
      evalReq : EvalRequest
    ) : Option[( AgentSessionURI, GloSExpr )] = {
      Some( ( evalReq.sessionURI, evalReq.expression ) )
    }
  }

  // The polling version of this request allows the client to pull
  // responses in at a rate that it can absorb; and then simply stop
  // evaluation when it is sated.
  case class evalPollRequest(
    override val sessionURI : AgentSessionURI,
    override val expression : GloSExpr
  ) extends EvaluatorMessage with EvalRequest

  case class evalNextPageRequest(
    val sessionURI : AgentSessionURI,
    val nextPage : AgentSessionURI
  ) extends EvaluatorMessage 

  // The subscription version of this request allows the server to
  // push results to the client.
  case class evalSubscribeRequest(
    override val sessionURI : AgentSessionURI,
    override val expression : GloSExpr
  ) extends EvaluatorMessage with EvalRequest
  
  // evalResponse( pageOfPosts, sessionURI )
  trait EvalResponse {
    def sessionURI : AgentSessionURI
    def pageOfPosts : Seq[Post]
  }

  object EvalResponse {
    def unapply(
      evalRsp : EvalResponse
    ) : Option[( AgentSessionURI, Seq[Post] )] = {
      Some( ( evalRsp.sessionURI, evalRsp.pageOfPosts ) )
    }
  }

  // Evaluation results are always paginated. In the polling version
  // of the conversation the client determines the rate at which pages
  // are delivered and dealt with. In the subscription version the
  // server does.
  case class evalPollResponse(    
    sessionURI : AgentSessionURI,
    pageOfPosts : Seq[Post],
    nextPage : AgentSessionURI
  ) extends EvaluatorMessage with EvalResponse

  case class evalSubscribeResponse(    
    sessionURI : AgentSessionURI,
    pageOfPosts : Seq[Post]
  ) extends EvaluatorMessage with EvalResponse
  
  // evalComplete( pageOfPosts, sessionURI )
  case class evalComplete(
    sessionURI : AgentSessionURI,
    pageOfPosts : Seq[Post]    
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
  
}

package usage {
  object EvaluatorMessageSet extends AbstractEvaluatorMessageSet {
    type Alias = String
    type Cnxn = String
    type Filter = String
    type Post = String
    type GloSExpr = String
  }
}
