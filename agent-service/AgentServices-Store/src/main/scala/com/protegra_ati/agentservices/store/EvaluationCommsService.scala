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
      onPost : Option[mTT.Resource] => Unit        
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

    def post[Value](
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      post[Value]( erql, erspl )( filter, cnxns, content, onPost )
    }

    def postV[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit        
    ) : Unit = {
      reset {
        node().publish( erql, InsertContentV( filter, cnxns, content ) )
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

    def postV[Value](
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      postV[Value]( erql, erspl )( filter, cnxns, content, onPost )
    }

    def put[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPut : Option[mTT.Resource] => Unit
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
    def put[Value](
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPut : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      put[Value]( erql, erspl )( filter, cnxns, content, onPut )
    }
    // TODO(metaweta): factor case class out of read, fetch, and feed
    def read(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onReadRslt : Option[mTT.Resource] => Unit,
      sequenceSubscription : Boolean
    ) : Unit = {      
      if ( sequenceSubscription ) {
        reset {
          for( e <- node().subscribe( erspl ) ) {
            e match {
              case None => {
                node().publish( erql, ReadExpr( filter, cnxns ) )
              }
              case _ => {
                onReadRslt( e )
              }
            }          
          }
        }
      }
      else {
        reset {
          node().publish( erql, ReadExpr( filter, cnxns ) )
        }
        reset {
          for( e <- node().subscribe( erspl ) ) {
            onReadRslt( e )          
          }
        }
      }
    }
    def read(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onReadRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) },
      sequenceSubscription : Boolean = false
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      read( erql, erspl )( filter, cnxns, onReadRslt, sequenceSubscription )
    }
    def fetch(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFetchRslt : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, FetchExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFetchRslt( e ) }
      }
    }
    def fetch(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFetchRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      fetch( erql, erspl )( filter, cnxns, onFetchRslt ) 
    }
    def feed(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFeedRslt : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, FeedExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFeedRslt( e ) }
      }
    }
    def feed(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFeedRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      feed(erql, erspl)( filter, cnxns, onFeedRslt )
    }
    def get(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onGetRslt : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, GetExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onGetRslt( e ) }
      }
    }
    def get(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onGetRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      get( erql, erspl )( filter, cnxns, onGetRslt )
    }
    def score(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      staff : Either[Seq[Cnxn],Seq[Label]],
      onScoreRslt : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, ScoreExpr( filter, cnxns, staff ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onScoreRslt( e ) }
      }
    }
    def score(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      staff : Either[Seq[Cnxn],Seq[Label]],
      onScoreRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      score( erql, erspl )( filter, cnxns, staff, onScoreRslt )
    }
    def cancel(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      connections : Seq[Cnxn],
      onCancel : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, CancelExpr( filter, connections ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onCancel( e ) }
      }
    }
    def cancel(
      filter : CnxnCtxtLabel[String,String,String],
      connections : Seq[Cnxn],
      onCancel : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "onCancel: optRsrc = " + optRsrc ) }
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      cancel( erql, erspl )( filter, connections, onCancel )
    }
    def runProcess(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      cmd : String,
      wkDir : Option[String],
      env : Seq[( String, String )],
      onExecution : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish( erql, RunProcessRequest( cmd, wkDir, env ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onExecution( e ) }
      }
    }
    def runProcess(
      cmd : String,
      wkDir : Option[String],
      env : Seq[( String, String )],
      onExecution : Option[mTT.Resource] => Unit
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      runProcess( erql, erspl )( cmd, wkDir, env, onExecution )
    }
  }

  def ensureServersConnected(
    pulseErql : CnxnCtxtLabel[String,String,String],
    pulseErspl : CnxnCtxtLabel[String,String,String]
  )(
    onConnection : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => { BasicLogService.tweet( "got response: " + optRsrc ) }
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
      case None | null => {    
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
