// -*- mode: Scala;-*- 
// Filename:    EvaluationCommsService.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  2 21:31:06 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import akka.actor._
import spray.routing._
import directives.CompletionMagnet
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._

import spray.httpx.encoding._

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.continuations._ 

import com.typesafe.config._

import java.util.Date
import java.util.UUID

trait EvaluationCommsService {
  self : EvalConfig =>

  import DSLCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._
  import DSLCommLinkCtor._
  import diesel.ConcreteHumanEngagement._
  import ConcreteHL._
  
  var _link : Option[
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ] = None

  def link() : Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] = {
    _link match {
      case Some( lnk ) => lnk
      case None => {
        val dslCommLinkHost =
          try {
            evalConfig.getString( "DSLCommLinkHost" )
          }
          catch {
            case e : Throwable => "10.0.1.10"
          }
        val dslCommLinkPort = 
          try {
            evalConfig.getInt( "DSLCommLinkPort" )
          }
          catch {
            case e : Throwable => 5672
          }
        val dslCommLinkRemoteHost = 
          try {
            evalConfig.getString( "DSLCommLinkRemoteHost" )
          }
          catch {
            case e : Throwable => "10.0.1.8"
          }
        val dslCommLinkRemotePort = 
          try {
            evalConfig.getInt( "DSLCommLinkRemotePort" )
          }
          catch {
            case e : Throwable => 5672
          }

        val lnk : Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] = DSLCommLinkCtor.link(
          dslCommLinkHost, dslCommLinkPort,
          dslCommLinkRemoteHost, dslCommLinkRemotePort
        )

        _link = Some( lnk )
        lnk
      }
    }    
  }

  trait AgentManager {
    def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def createAgent(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      selfCnxn : Cnxn,
      thisUser : User[String,String,String],
      onCreation : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        link.publish( erql, InsertContent( filter, List( selfCnxn ), thisUser ) )
      }
      reset {
        for( e <- link.subscribe( erspl ) ) { onCreation( e ) }
      }
    }
    def post[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        link.publish( erql, InsertContent( filter, cnxns, content ) )
      }
      reset {
        for( e <- link.subscribe( erspl ) ) { onPost( e ) }
      }
    }
    def feed(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFeedRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        link.publish( erql, FeedExpr( filter, cnxns ) )
      }
      reset {
        for( e <- link.subscribe( erspl ) ) { onFeedRslt( e ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        link.publish( erql, ScoreExpr( filter, cnxns, staff ) )
      }
      reset {
        for( e <- link.subscribe( erspl ) ) { onScoreRslt( e ) }
      }
    }
  }

  @transient
  var _agentMgr : Option[AgentManager with Serializable] = None
  def agentMgr() : AgentManager with Serializable = {
    _agentMgr match {
      case Some( agntMgr ) => agntMgr
      case None => {
        val agntMgr =
          new AgentManager with Serializable {
            override def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalRequestLabel()(
                sessionID.toString
              ).getOrElse( throw new Exception( "unable to make evalRequestLabel" ) )
            }
            override def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalResponseLabel()(
                sessionID.toString
              ).getOrElse( throw new Exception( "unable to make evalResponseLabel" ) )
            }
          }

        _agentMgr = Some( agntMgr )

        agntMgr
      }
    }    
  }
}
