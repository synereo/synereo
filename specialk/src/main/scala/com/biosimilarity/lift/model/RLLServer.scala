// -*- mode: Scala;-*- 
// Filename:    RLLServer.scala 
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
import Being._

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import scala.util.continuations._ 

import java.net.URI
import java.util.UUID
import java.io.StringReader

class RLLEvaluationService(
  override val exchange : PersistedStringMGJ
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
    for( exprRsrc <- exchange.get( evaluationRequestStr ) ) {
      exprRsrc match {
	case Some( mTT.Ground( expr ) ) => {
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	  println( "received evaluation request from: " + endPtId )
	  println( "expr: " + expr )
	  println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	  exchange.put( evaluationResponseStr, evaluation( expr ) )
	}
	case Some( mTT.RBoundHM( Some( mTT.Ground( expr ) ), _ ) ) => {
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

  def evalVM = {
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting to serve session request: " + endPointId.toString )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    reset {
      for( endPtRsrc <- exchange.get( sessionRequestStr ) ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received session request from: " + endPtId )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    inSession(
	      endPtId,
	      "vmEvaluationRequest",
	      "vmEvaluationResponse",
	      ( expr : String ) => { new RLLREPL( ).eval( expr ) }
	    )
	  }
	  case Some( mTT.RBoundHM( Some( mTT.Ground( endPtId ) ), _ ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "received session request from: " + endPtId )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    inSession(
	      endPtId,
	      "vmEvaluationRequest",
	      "vmEvaluationResponse",
	      ( expr : String ) => { new RLLREPL( ).eval( expr ) }
	    )
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }

  def evalX = {
    reset {
      for( endPtRsrc <- exchange.get( sessionRequestStr ) ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    inSession(
	      endPtId,
	      "experimentalEvaluationRequest",
	      "experimentalEvaluationResponse",
	      ( expr : String ) => { new RLLREPL( ).eval( expr ) }
	    )
	  }
	  case Some( mTT.RBoundHM( Some( mTT.Ground( endPtId ) ), _ ) ) => {
	    inSession(
	      endPtId,
	      "vmEvaluationRequest",
	      "vmEvaluationResponse",
	      ( expr : String ) => { new RLLREPL( ).eval( expr ) }
	    )
	  }
	  case _ => {
	    println( "waiting for session request ..." )
	  }
	}	
      }
    }    
  }
}
