// -*- mode: Scala;-*- 
// Filename:    RLambdaServer.scala 
// Authors:     lgm                                                    
// Creation:    Sun Oct 16 18:51:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._
import SyntaxConversion._

import com.biosimilarity.lift.model.store.usage._
import PersistedMonadicTS._

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import scala.util.continuations._ 

import org.prolog4j._

import java.net.URI
import java.util.UUID
import java.io.StringReader

class RLambdaEvaluationService(
  override val exchange : PersistedtedStringMGJ
) extends RLLEvalProtocol( exchange ) {  
  val sessionMap = new HashMap[String,String]( )

  def sessionMsg( sessId : String, msgType : String ) : String = {
    (
      msgType + "( "
      //+ "sessionId( "
      + "\"" + sessId + "\""
      //+ " )"
      + " )"
    )
  }

  def inSession(
    endPtId : String,
    reqType : String,
    rspType : String,
    evaluation : String => String
  ) = {
    val sessionResponseStr =
      "sessionResponse( " + "\"" + endPtId + "\"" + " )"      
    val sessId = getUUID
    sessionMap += ( ( sessId.toString, endPtId ) )
    exchange.put( sessionResponseStr, sessId.toString )
    
    val ( evaluationRequestStr, evaluationResponseStr ) = 
      (
	sessionMsg( sessId.toString, reqType ),
	sessionMsg( sessId.toString, rspType )
      );

    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting to serve evaluation request: " )
    println( "client id: " + endPtId )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

    reset {
      for( exprRsrc <- exchange.get( evaluationRequestStr ) ) {
	exprRsrc match {
	  case Some( mTT.Ground( expr ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received evaluation request from: " + endPtId )
	    println( "expr: " + expr )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    exchange.put( evaluationResponseStr, evaluation( expr ) )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( expr ) ), _ ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received evaluation request from: " + endPtId )
	    println( "expr: " + expr )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    exchange.put( evaluationResponseStr, evaluation( expr ) )
	  }
	  case _ => {
	    println( "waiting for evaluation request..." )
	  }
	}      
      }
    }
  }

  def evalInSession( endPtId : String ) = {
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "received session request from: " + endPtId )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    inSession(
      endPtId,
      "evaluationRequest",
      "evaluationResponse",
      ( expr : String ) => { new RLambdaREPL( ).eval( expr ) }
    )
  }

  def serve = {
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting to serve session request: " + endPointId.toString )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    
    reset {
      for( endPtRsrc <- exchange.get( sessionRequestStr ) ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    evalInSession( endPtId )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( endPtId ) ), _ ) ) => {
	    evalInSession( endPtId )	    
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
  override val exchange : PersistedtedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( appEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def appEvalReqStr : String = {
    throw new Exception( "tbd" )
  }
  override def serve = {
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
  override val exchange : PersistedtedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( absEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def absEvalReqStr : String = {
    throw new Exception( "tbd" )
  }  
  override def serve = {
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
  override val exchange : PersistedtedStringMGJ
) extends RLambdaEvaluationService( exchange ) {
  def evalInSession( mntnEvalReq : String, subst : Solution[String] ) = {
    throw new Exception( "tbd" )
  }
  def mntnEvalReqStr : String = {
    throw new Exception( "tbd" )
  }  
  override def serve = {
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
