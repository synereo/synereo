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

trait UseCaseHelper extends AgentCnxnTypes with CnxnString[String,String,String] with Serializable {
  import com.protegra_ati.agentservices.store.extensions.StringExtensions._
  import DSLCommLinkCtor._
  @transient
  val ( client1, server1 ) = stdBiLink()
  @transient
  val sessionMap =
    new HashMap[String,( Either[ConcreteHL.HLExpr,ConcreteHL.HLExpr], Option[ConcreteHL.HLExpr] )]()
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
  def doPutBottomRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( ConcreteHL.Bottom ), None ) )

    reset {      
      client1.put( erqlChan, DSLCommLink.mTT.Ground( ConcreteHL.Bottom ) )
    }
  }
  def doPutFeedRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val feedLabelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )"
    val feedLabel =
      fromTermString(
	feedLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse feed label" + feedLabelStr )
      )
    val feedCnxn =
      AgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 
    val feedExpr =
      ConcreteHL.FeedExpr( feedLabel, List( feedCnxn ) )
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( feedExpr ), None ) )       

    reset {
      client1.put(
	erqlChan,
	DSLCommLink.mTT.Ground(
	  feedExpr
	)
      )
    }
  }
  def doPutScoreRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val scoreLabelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )"
    val scoreLabel =
      fromTermString(
	scoreLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse score label" + scoreLabelStr )
      )
    val scoreCnxn =
      AgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 
    val scoreExpr =
      ConcreteHL.ScoreExpr(
	scoreLabel,
	List( scoreCnxn ),
	Left[Seq[ConcreteHL.Cnxn],Seq[ConcreteHL.Label]]( List( scoreCnxn ) )
      )
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( scoreExpr ), None ) )

    reset {
      client1.put(
	erqlChan,
	DSLCommLink.mTT.Ground(
	  scoreExpr
	)
      )
    }
  }
  def doPutPostRequest( sessionId : String = UUID.randomUUID.toString() ) = {
    val postLabelStr : String = "myLife( inTheBush( ofGhosts( true ) ) )"
    val postLabel =
      fromTermString( 
	postLabelStr
      ).getOrElse(
	throw new Exception( "failed to parse post label" + postLabelStr )
      )
    val postCnxn =
      AgentCnxn("Jerry.Seinfeld".toURI, "", "Jerry.Seinfeld".toURI) 
    val postExpr =
      ConcreteHL.InsertContent[String](
	postLabel,
	List( postCnxn ),
	"David Byrne"
      )
    val ( _, erqlChan ) = erql( sessionId ) 

    sessionMap += ( sessionId -> ( Left[ConcreteHL.HLExpr,ConcreteHL.HLExpr]( postExpr ), None ) )

    reset {
      client1.put(
	erqlChan,
	DSLCommLink.mTT.Ground(
	  postExpr
	)
      )
    }
  }

  def doGetRequest( sessionId : String = "SessionId" ) = {
    val ( _, erqlChan ) = erql( sessionId ) 

    reset {
      for( e <- client1.subscribe( erqlChan ) ) {
	println( e )
      }
    }
  }
  
  def doGetFeedResponse( sessionId : String = "SessionId" ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.FeedExpr( label, cnxns ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- client1.subscribe( ersplChan ) ) {
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
  def doGetScoreResponse( sessionId : String = "SessionId" ) = {    
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.ScoreExpr( label, cnxns, staff ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- client1.subscribe( ersplChan ) ) {
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
  def doGetPostResponse( sessionId : String = "SessionId" ) = {
    for( eitherExpr <- sessionMap.get( sessionId ) ) {
      eitherExpr match {
	case ( Left( expr@ConcreteHL.InsertContent( label, cnxns, content ) ), None ) => {
	  val ( _, ersplChan ) = erspl( sessionId ) 
	  reset {
	    for( e <- client1.subscribe( ersplChan ) ) {
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
