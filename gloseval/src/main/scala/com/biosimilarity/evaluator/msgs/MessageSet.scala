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

object MessageSet {
  type Alias = String
  type Cnxn = String
  type Filter = String
  type Post = String
  type GloSExpr = String

  trait EvaluatorMessage

  // initializeSessionRequest( sessionURI )
  // agentURI ::= agent://[user[:pwd]@]host[:port]/agentId
  case class initializeSessionRequest(
    agentURI : AgentURI
  ) extends EvaluatorMessage

  // initializeSessionResponse(
  //   listOfAliases, defaultAlias, listOfLabels, listOfCnxns,
  //   lastActiveFilter,
  //   sessionURI
  // )
  // sessionURI ::= agent-session://[userToken@]sessionId[/subsessionId]*[?parm=value[,parm=value]*]
  case class initializeSessionResponse(
    sessionURI : AgentSessionURI,
    listOfAliases : Seq[Alias], defaultAlias : Alias,
    listOfLabels : Seq[Alias], listOfCnxns : Seq[Cnxn],
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
  
  // evalRequest( expression, sessionURI )
  case class evalRequest(
    sessionURI : AgentSessionURI,
    expression : GloSExpr    
  ) extends EvaluatorMessage
  
  // evalResponse( pageOfPosts, sessionURI )
  case class evalResponse(    
    sessionURI : AgentSessionURI,
    pageOfPosts : Seq[Post]
  ) extends EvaluatorMessage
  
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
