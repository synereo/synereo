// -*- mode: Scala;-*- 
// Filename:    RLambdaServer.scala 
// Authors:     lgm                                                    
// Creation:    Sun Oct 16 18:51:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._
import SyntaxConversion._

import com.biosimilarity.lift.model.store.usage._
import PersistedMonadicTS._
import Being._

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import scala.util.continuations._ 

import org.prolog4j._

import java.net.URI
import java.util.UUID
import java.io.StringReader

class RLambdaEvaluationService(
  override val exchange : PersistedStringMGJ
) extends RLLEvalProtocol( exchange )
with DEvalMsgScope
with CnxnXML[String,String,String]
with CnxnCtxtInjector[String,String,String]
with Blobify
with UUIDOps {  
  type EvalMsgTypes = DEvalMessages
  
  object theDEvalMsgs extends DEvalMessages {
      val sessionID = getUUID
      val flowID = getUUID
      val expr = "lambda x.x"
    def protoSessionRequest =
      new sessionRequest( sessionID.toString )
    def protoSessionResponse =
      new sessionResponse( sessionID.toString )
    def protoEvaluationRequest = 
      new evalExprRequest(
	sessionID.toString,
	flowID.toString,
	expr
      )
    def protoEvaluationResponse =
      new evalExprResponse(
	sessionID.toString,
	flowID.toString,
	expr
      )       
  }

  def protoEvalMsgs = theDEvalMsgs  

  object protocolConversions {
    implicit def caseClassAsPattern(
      cc : ScalaObject with Product with Serializable
    ) : CnxnCtxtLabel[String,String,String] with Factual = {
      val l2ns : String => String = 
	( s : String ) => {
	  s.substring( 0, 1 ).toLowerCase + s.substring( 1, s.length )
	}
      val v2t : java.lang.Object => String = 
	( o : java.lang.Object ) => {
	  if ( o.isInstanceOf[String] )
	    { "\"" + o + "\"" }
	  else { o + "" }
	}
      fromCaseClass( l2ns, v2t )( cc )
    }
  }

  case class EvalSessionRsrc(
    endPtId : String,
    flowVar : String,
    exprVar : String
  )
  
  val sessionMap = new HashMap[String,String]( )
  val protocolSessionMap = new HashMap[String,EvalSessionRsrc]( )

  // Using the protocol methods
  def makeVariable( root : String ) : String = {
    ( root + getUUID ).replace( "-", "" )
  }
  def inSessionProtocol(
    endPtId : String,
    evaluation : String => String
  ) = {
    import protocolConversions._

    val sessId = getUUID    
    val flowIdVar = makeVariable( "flow" )
    val exprVar = makeVariable( "expr" )

    val evalSessionRsrc =
      EvalSessionRsrc( endPtId, flowIdVar, exprVar )

    protocolSessionMap += ( ( sessId.toString, evalSessionRsrc ) )
    exchange.put( caseClassAsPattern( theDEvalMsgs.sessionResponse( endPtId ) ), sessId.toString )    

    val evalExprReq =
      theDEvalMsgs.evalExprRequest( sessId.toString, flowIdVar, exprVar )
    
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting to serve evaluation request: " )
    println( "client id: " + endPtId )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

    reset {
      for( evalExprReqK <- exchange.get( caseClassAsPattern( evalExprReq ) ) ) {
	evalExprReqK match {
	  case Some( mTT.Ground( expr ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received malformed evaluation request from: " + endPtId )
	    println( "expr: " + expr )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    throw new Exception( "evaluation request with no binding" )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( expr ) ), None ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received malformed evaluation request from: " + endPtId )
	    println( "expr: " + expr )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    throw new Exception( "evaluation request with no binding" )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( expr ) ), Some( soln ) ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received evaluation request from: " + endPtId )
	    println( "expr: " + expr )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    
	    val evalExprRsp =
	      theDEvalMsgs.evalExprResponse(
		sessId.toString,
		soln.get( flowIdVar ),
		expr
	      )

	    exchange.put( caseClassAsPattern( evalExprRsp ), evaluation( soln.get( exprVar ) ) )
	  }
	  case _ => {
	    println( "waiting for evaluation request..." )
	  }
	}      
      }
    }
  }

  def evalInSessionProtocol( endPtId : String ) = {
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "received session request from: " + endPtId )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    inSessionProtocol(
      endPtId,
      ( expr : String ) => { new RLambdaREPL( ).eval( expr ) }
    )
  }

  def serveProtocol = {
    import protocolConversions._

    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting to serve session request: " + endPointId.toString )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    
    reset {
      for(
	endPtRsrc <- exchange.get(
	  caseClassAsPattern(
	    theDEvalMsgs.sessionRequest( makeVariable( "session" ) )
	  )
	)
      ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    evalInSessionProtocol( endPtId )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( endPtId ) ), _ ) ) => {
	    evalInSessionProtocol( endPtId )
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }
}

class RLambdaApplicationEvaluationService(
  override val exchange : PersistedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( appEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def appEvalReqStr : String = {
    throw new Exception( "tbd" )
  }
  override def serveProtocol = {
    reset {
      for( appEvalRsrc <- exchange.get( appEvalReqStr ) ) {
	appEvalRsrc match {
	  case Some( mTT.Ground( appEvalReq ) ) => {
	    throw new Exception( "invalid appEvalReq format : " + appEvalReq )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( appEvalReq ) ), Some( subst ) ) ) => {
	    evalInSession( appEvalReq, subst )	    
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }
}

class RLambdaAbstractionEvaluationService(
  override val exchange : PersistedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( absEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def absEvalReqStr : String = {
    throw new Exception( "tbd" )
  }  
  override def serveProtocol = {
    reset {
      for( absEvalRsrc <- exchange.get( absEvalReqStr ) ) {
	absEvalRsrc match {
	  case Some( mTT.Ground( absEvalReq ) ) => {
	    throw new Exception( "invalid absEvalReq format : " + absEvalReq )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( absEvalReq ) ), Some( subst ) ) ) => {
	    evalInSession( absEvalReq, subst )	    
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }
}

class RLambdaMentionEvaluationService(
  override val exchange : PersistedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( mntnEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def mntnEvalReqStr : String = {
    throw new Exception( "tbd" )
  }  
  override def serveProtocol = {
    reset {
      for( mntnEvalRsrc <- exchange.get( mntnEvalReqStr ) ) {
	mntnEvalRsrc match {
	  case Some( mTT.Ground( mntnEvalReq ) ) => {
	    throw new Exception( "invalid mntnEvalReq format : " + mntnEvalReq )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( mntnEvalReq ) ), Some( subst ) ) ) => {
	    evalInSession( mntnEvalReq, subst )	    
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }
}
