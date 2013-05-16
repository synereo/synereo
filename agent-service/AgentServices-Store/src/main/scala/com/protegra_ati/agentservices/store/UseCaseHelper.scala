// -*- mode: Scala;-*- 
// Filename:    UseCaseHelper.scala 
// Authors:     lgm                                                    
// Creation:    Mon May 13 13:57:26 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.lift.model.store._

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import java.util.UUID

trait ChannelGeneration {
  def erql( sessionId : String = UUID.randomUUID().toString ) : ( String, CnxnCtxtLabel[String,String,String] ) = {
    (
      sessionId,
      DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()( sessionId.toString ).getOrElse( 
	throw new Exception( "error making evalRequestLabel" )
      )
    )
  }
  def erspl( sessionId : String = "_" ) : ( String, CnxnCtxtLabel[String,String,String] ) = {
    (
      sessionId,
      DSLCommLinkCtor.ExchangeLabels.evalResponseLabel()( sessionId ).getOrElse( 
	throw new Exception( "error making evalRequestLabel" )
      )
    )
  }
}

trait MessageGeneration extends CnxnString[String,String,String] {
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._

  def defaultLabelStr = "myLife( inTheBush( ofGhosts( true ) ) )"
  def mkFeedExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.FeedExpr = {
    val feedLabelStr : String = labelStr
    val feedLabel =
      fromTermString(
	feedLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse feed label" + feedLabelStr )
      )
    val feedCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 

    ConcreteHL.FeedExpr( feedLabel, List( feedCnxn ) )      
  }
  def mkScoreExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.ScoreExpr = {
    val scoreLabelStr : String = labelStr
    val scoreLabel =
      fromTermString(
	scoreLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse score label" + scoreLabelStr )
      )
    val scoreCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 

    ConcreteHL.ScoreExpr(
      scoreLabel,
      List( scoreCnxn ),
      Left[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]( List( scoreCnxn ) )
    )          
  }
  def mkPostExpr( labelStr : String = defaultLabelStr ) : ConcreteHL.InsertContent[String] = {
    val postLabelStr : String = labelStr
    val postLabel =
      fromTermString( 
	postLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse post label" + postLabelStr )
      )
    val postCnxn =
      ConcreteHL.PortableAgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 
    ConcreteHL.InsertContent[String](
      postLabel,
      List( postCnxn ),
      "David Byrne"
    )          
  }  
}

object CommManagement {
  import DSLCommLinkCtor._
  @transient
  lazy val ( client1, server1 ) = stdBiLink()
}

trait StorageManagement {
  import CommManagement._
  def doDrop() = {
    import com.biosimilarity.lift.model.store.mongo._
    val clntSess1 =
      MongoClientPool.client( client1.cache.sessionURIFromConfiguration )
    val mcExecLocal =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocolLocal" )
    val mcExecRemote =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocolRemote" )
    val mcExec =
      clntSess1.getDB( client1.cache.defaultDB )( "DSLExecProtocol" )

    mcExecLocal.drop
    mcExecRemote.drop
    mcExec.drop
  }
}

trait ExerciseHLDSL {  
  self : ChannelGeneration with MessageGeneration with AgentCnxnTypes =>
  import CommManagement._
  import DSLCommLinkCtor._

  @transient
  val sessionMap =
    new HashMap[String,( Either[ConcreteHL.HLExpr,ConcreteHL.HLExpr], Option[ConcreteHL.HLExpr] )]()

  implicit def toAgentCnxn( pAC : ConcreteHL.PortableAgentCnxn ) : AgentCnxn = {
    new AgentCnxn( pAC.src, pAC.label, pAC.trgt )
  }     
  def doPutBottomRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( ConcreteHL.Bottom ), None ) )

    reset {      
      client1.put( erqlChan, DSLCommLink.mTT.Ground( ConcreteHL.Bottom ) )
    }
  }
  def doPutHLExprRequest(
    node : StdEvaluationRequestChannel,
    sessionId : String,
    expr : ConcreteHL.HLExpr
  ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None ) )       

    reset { node.put( erqlChan, DSLCommLink.mTT.Ground( expr ) ) }
  }
  def doPutFeedRequest(
    node : StdEvaluationRequestChannel = client1,
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkFeedExpr( labelStr ) )
  }
  def doPutScoreRequest(
    node : StdEvaluationRequestChannel = client1,
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkScoreExpr( labelStr ) )
  }
  def doPutPostRequest(
    node : StdEvaluationRequestChannel = client1,
    labelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )",
    sessionId : String = UUID.randomUUID.toString()
  ) = {
    doPutHLExprRequest( node, sessionId, mkPostExpr( labelStr ) )
  }

  def doGetRequest( 
    node : StdEvaluationRequestChannel = client1,
    sessionId : String = "SessionId" 
  ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    reset {
      for( e <- node.subscribe( erqlChan ) ) {
	println( e )
      }
    }
  }
  
  def doGetFeedResponse(
    node : StdEvaluationRequestChannel = client1,
    sessionId : String = "SessionId"
  ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.FeedExpr( label, cnxns ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
  def doGetScoreResponse(
    node : StdEvaluationRequestChannel = client1,
    sessionId : String = "SessionId" 
  ) = {    
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.ScoreExpr( label, cnxns, staff ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
  def doGetPostResponse(
    node : StdEvaluationRequestChannel = client1,
    sessionId : String = "SessionId"
  ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.InsertContent( label, cnxns, content ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- node.subscribe( ersplChan ) ) {
	      val rslt = ( Right[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( expr ), None )
	      sessionMap += ( sessionId -> rslt );
	      ()
	    }
	  }
	}
	case ( Left( expr ), _ ) => {
	  throw new Exception( "unexpected expression type: " + expr )
	}
	case ( Right( expr ), _ ) => {
	  println( "session closed" )
	}
      }      
    }
  }
}

trait UseCaseHelper extends MessageGeneration
 with ChannelGeneration
 with StorageManagement
 with ExerciseHLDSL
 with AgentCnxnTypes
 with CnxnString[String,String,String] {
}

package usage {
  object SimpleClient
    extends EvaluationCommsService  
     with MessageGeneration
     with ChannelGeneration
     with EvalConfig
     with StorageManagement
     with Serializable
  {
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  }
  
  object MyHLDSLTrampoline
    extends ExerciseHLDSL
     with ChannelGeneration
     with MessageGeneration
     with AgentCnxnTypes
}
