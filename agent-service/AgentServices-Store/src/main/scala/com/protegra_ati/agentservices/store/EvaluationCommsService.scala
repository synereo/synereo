// -*- mode: Scala;-*- 
// Filename:    EvaluationCommsService.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  2 21:31:06 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import java.net.URI
import java.util.Date
import java.util.UUID



trait EvaluationCommsService extends CnxnString[String, String, String]{
  self : EvalConfig with DSLCommLinkConfiguration =>

  import DSLCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._
  import DSLCommLinkCtor._
  import diesel.ConcreteHumanEngagement._
  import ConcreteHL._
  
  var _clientServerPair : Option[
    (
      Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse],
      Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
    )
  ] = None

  var _node : Option[
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ] = None 

  def link() : (
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse],
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ) = {
    _clientServerPair match {
      case Some( lnk ) => lnk
      case None => {        
        val ( client, server ) : ( Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ], Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] ) = DSLCommLinkCtor.stdBiLink(
          clientHostName, clientPort,
          serverHostName, serverPort
        )

        _clientServerPair = Some( ( client, server ) )
        ( client, server )
      }
    }    
  }

  def node(
    flip : Boolean = false
  ) : Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] = {
    _node match {
      case Some( n ) => n
      case None => {        
        val n : Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] = DSLCommLinkCtor.stdLink(
          serverHostName, serverPort,
          clientHostName, clientPort
        )( flip )

        _node = Some( n )
        n
      }
    }    
  }

  trait AgentManager {
    def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def makePolarizedPair() = {
      val sessionID = UUID.randomUUID
      (erql( sessionID ), erspl( sessionID ))
    }

    def adminErql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def adminErspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def makePolarizedAdminPair() = {
      val sessionID = UUID.randomUUID
      (adminErql( sessionID ), adminErspl( sessionID ))
    }

    def post[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, InsertContent( filter, cnxns, content ) )
      }
      reset {
        try { 
          for(
            e <- try { node().subscribe( erspl ) }
            catch {
              case e : Throwable => {
                BasicLogService.tweetTrace( e.asInstanceOf[Exception] )
                throw e
              }
            }
          ) {
            onPost( e )
          }
        }
        catch {
          case e : Throwable => {
            BasicLogService.tweetTrace( e.asInstanceOf[Exception] )
            throw e
          }
        }
      }
    }
    def put[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPut : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, PutContent( filter, cnxns, content ) )
      }
      reset {
        try { 
          for(
            e <- try { node().subscribe( erspl ) }
            catch {
              case e : Throwable => {
                BasicLogService.tweetTrace( e.asInstanceOf[Exception] )
                throw e
              }
            }
          ) {
            onPut( e )
          }
        }
        catch {
          case e : Throwable => {
            BasicLogService.tweetTrace( e.asInstanceOf[Exception] )
            throw e
          }
        }
      }
    }
    // TODO(metaweta): factor case class out of read, fetch, and feed
    def read(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onReadRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, ReadExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onReadRslt( e ) }
      }
    }
    def fetch(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFetchRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, FetchExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFetchRslt( e ) }
      }
    }
    def feed(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFeedRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, FeedExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFeedRslt( e ) }
      }
    }
    def get(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onGetRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, GetExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onGetRslt( e ) }
      }
    }
    def score(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      staff : Either[Seq[Cnxn],Seq[Label]],
      onScoreRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, ScoreExpr( filter, cnxns, staff ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onScoreRslt( e ) }
      }
    }
  }

  def ensureServersConnected(
    pulseErql : CnxnCtxtLabel[String,String,String],
    pulseErspl : CnxnCtxtLabel[String,String,String]
  )(
    onConnection : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
  ) : Unit = {
    // post to channel
    reset {
      node().put( pulseErql, ConcreteHL.Bottom )
    }
    
    // wait for response
    reset {
      for( e <- node().get( pulseErspl ) ) {
        onConnection( e )
      }
    }
  }

  @transient
  var _agentMgr : Option[AgentManager with Serializable] = None
  def agentMgr( flip : Boolean = false ) : AgentManager with Serializable = {
    _agentMgr match {
      case Some( agntMgr ) => agntMgr
      case None => {    
        node( flip )
        val agntMgr =
          new AgentManager with Serializable {
            override def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalRequestLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make evalRequestLabel" ) )
            }
            override def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalResponseLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make evalResponseLabel" ) )
            }
            override def adminErql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.adminRequestLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make adminRequestLabel" ) )
            }
            override def adminErspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.adminResponseLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make adminResponseLabel" ) )
            }
          }

        _agentMgr = Some( agntMgr )

        agntMgr
      }
    }    
  }
}

package usage {
  object EvaluationServiceContext
  extends Serializable 
  with UseCaseHelper {    
    @transient
    lazy val eServe =
      new EvaluationCommsService
         with EvalConfig
         with DSLCommLinkConfiguration
         with Serializable {
         }    
  }  
}
