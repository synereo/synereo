// -*- mode: Scala;-*- 
// Filename:    RLLClient.scala 
// Authors:     lgm                                                    
// Creation:    Sun Oct 16 21:30:56 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._

import com.biosimilarity.lift.model.store.usage._
import PersistedMonadicTS._

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._
import scala.util.continuations._ 
  
import java.net.URI
import java.util.UUID
import java.io.StringReader

class RLLEvaluationServiceProxy(
  override val exchange : PersistedtedStringMGJ
) extends RLLEvalProtocol( exchange ) {
  def evalExchange(
    str : String,
    sessId : String,
    k : Option[mTT.Resource] => Unit
  ) = {
    val evaluationRequestStr = 
      (
	"vmEvaluationRequest( "
	//+ "sessionId( "
	+ "\"" + sessId + "\""
	//+ " )"
	+ " )"
      );
    val evaluationResponseStr = 
      (
	"vmEvaluationResponse( "
	//+ "sessionId( "
	+ "\"" + sessId + "\""
	//+ " )"
	+ " )"
      );
    
    exchange.put( evaluationRequestStr, str )
    
    for( rslt <- exchange.get( evaluationResponseStr ) ) {
      println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
      println( "received evaluation response: " )
      println( "session id: " + sessId )
      println( "expression: " + str )
      println( "evaluation result: " + rslt )
      println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
      k( rslt )
    }
  }

  def evalRemoteVM( str : String, k : Option[mTT.Resource] => Unit ) = {
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "making session request: " + endPointId.toString )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

    reset {
      exchange.put( "sessionRequest( \"go\" )", endPointId.toString )
    }

    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
    println( "waiting for session response: " + endPointId.toString )
    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

    reset {
      for( sessRsrc <- exchange.get( sessionResponseStr ) ) {
	println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	println( "received session response: " + sessRsrc )
	println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

	sessRsrc match {
	  case Some( mTT.Ground( sessId ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "engaging evaluation exchange: " )
	    println( "client: " + endPointId.toString )
	    println( "session id: " + sessId )
	    println( "expression: " + str )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    evalExchange( str, sessId, k )
	  }
	  case Some( mTT.RBound( Some( mTT.Ground( sessId ) ), _ ) ) => {
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "engaging evaluation exchange: " )
	    println( "client: " + endPointId.toString )
	    println( "session id: " + sessId )
	    println( "expression: " + str )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    evalExchange( str, sessId, k )
	  }
	  case _ => {
	    println( "waiting for session response..." )
	  }
	}	
      }
    }
  }
  
  def evalRemoteX( str : String, k : Option[mTT.Resource] => Unit ) = {
    reset {
      exchange.put( sessionRequestStr, endPointId.toString )
    }
    reset {
      for( sessId <- exchange.get( sessionResponseStr ) ) {
	val evaluationRequestStr = 
	  (
	    "experimentalEvaluationRequest( "
	    + "sessionId( " + sessId + " )"
	    + " )"
	  );
	val evaluationResponseStr = 
	  (
	    "experimentalEevaluationResponse( "
	    + "sessionId( " + sessId + " )"
	    + " )"
	  );
	
	exchange.put( evaluationRequestStr, str )
	
	for( rslt <- exchange.get( evaluationResponseStr ) ) {
	  k( rslt )
	}
      }
    }
    
  }
}
