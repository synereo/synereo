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

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

import java.net.URI
import java.util.UUID
import java.io.StringReader

object RLLEvaluationServiceProxy extends RLLEvaluationProtocol {
  import com.biosimilarity.lift.model.store.usage._
  import PersistedMonadicTS._
  import scala.util.continuations._ 

  def evalRemoteVM( str : String, k : Option[mTT.Resource] => Unit ) = {
    reset {
      exchange.put( sessionRequestStr, endPointId.toString )
    }
    reset {
      for( sessId <- exchange.get( sessionResponseStr ) ) {
	val evaluationRequestStr = 
	  (
	    "vmEvaluationRequest( "
	    + "sessionId( " + sessId + " )"
	    + " )"
	  );
	val evaluationResponseStr = 
	  (
	    "vmEevaluationResponse( "
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
