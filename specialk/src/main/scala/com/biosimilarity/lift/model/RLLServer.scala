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

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._

import java.net.URI
import java.util.UUID
import java.io.StringReader

object RLLEvaluationService extends RLLEvaluationProtocol {
  import com.biosimilarity.lift.model.store.usage._
  import PersistedMonadicTS._
  import scala.util.continuations._ 

  val sessionMap = new HashMap[String,String]( )

  def sessionMsg( sessId : String, msgType : String ) : String = {
    (
      msgType + "( "
      + "sessionId( " + sessId + " )"
      + " )"
    )
  }

  def inSession(
    endPtId : String,
    reqType : String,
    rspType : String,
    evaluation : String => String
  ) = {
    val sessionResponseStr = "sessionResponse( " + endPtId + " )"      
    val sessId = getUUID
    sessionMap += ( ( sessId.toString, endPtId ) )
    exchange.put( sessionResponseStr, sessId.toString )
    
    val ( evaluationRequestStr, evaluationResponseStr ) = 
      (
	sessionMsg( sessId.toString, reqType ),
	sessionMsg( sessId.toString, rspType )
      );
    
    for( exprRsrc <- exchange.get( evaluationRequestStr ) ) {
      exprRsrc match {
	case Some( mTT.Ground( expr ) ) => {
	  exchange.put( evaluationResponseStr, evaluation( expr ) )
	}
	case Some( mTT.RBound( Some( mTT.Ground( expr ) ), _ ) ) => {
	  exchange.put( evaluationResponseStr, evaluation( expr ) )
	}
	case _ => {
	  throw new Exception( "not handling evaluation request: " + exprRsrc )
	}
      }      
    }
  }

  def evalVM( str : String, k : Option[mTT.Resource] => Unit ) = {
    reset {
      for( endPtRsrc <- exchange.get( sessionRequestStr ) ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    inSession(
	      endPtId,
	      "vmEvaluationRequest",
	      "vmEvaluationResponse",
	      ( expr : String ) => {
		new RLLREPL( ).eval( expr )
	      }
	    )
	  }
	  case _ => {
	    throw new Exception( "not handling session request: " + endPtRsrc )
	  }
	}	
      }
    }    
  }

  def evalX( str : String, k : Option[mTT.Resource] => Unit ) = {
    reset {
      for( endPtRsrc <- exchange.get( sessionRequestStr ) ) {
	endPtRsrc match {
	  case Some( mTT.Ground( endPtId ) ) => {
	    inSession(
	      endPtId,
	      "experimentalEvaluationRequest",
	      "experimentalEvaluationResponse",
	      ( expr : String ) => {
		new RLLREPL( ).eval( expr )
	      }
	    )
	  }
	  case _ => {
	    throw new Exception( "not handling session request: " + endPtRsrc )
	  }
	}	
      }
    }    
  }
}
