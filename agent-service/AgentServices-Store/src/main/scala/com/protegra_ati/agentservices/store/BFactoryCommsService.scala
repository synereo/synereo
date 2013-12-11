// -*- mode: Scala;-*- 
// Filename:    BFactoryCommsService.scala 
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



trait BFactoryCommsService extends CnxnString[String, String, String]{
  self : EvalConfig with BFactoryCommLinkConfiguration =>

  import BFactoryCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._
  import BFactoryCommLinkCtor._
  import diesel.ConcreteHumanEngagement._
  import ConcreteBFactHL._
  
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
        ] ) = BFactoryCommLinkCtor.stdBiLink(
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
        ] = BFactoryCommLinkCtor.stdLink(
          serverHostName, serverPort,
          clientHostName, clientPort
        )( flip )

        _node = Some( n )
        n
      }
    }    
  }

  trait BFactoryManager {
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

    def mapBehavior(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      cnxn : Cnxn,
      label : CnxnCtxtLabel[String,String,String],
      behavior : String,
      onMap : Option[mTT.Resource] => Unit        
    ) : Unit = {
      reset {
        node().publish( erql, MapBehavior( cnxn, label, behavior ) )
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
            onMap( e )
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

    def mapBehavior[Value](
      cnxn : Cnxn,
      label : CnxnCtxtLabel[String,String,String],
      behavior : String,
      onMap : Option[mTT.Resource] => Unit        
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      mapBehavior( erql, erspl )( cnxn, label, behavior, onMap )
    }

    def commenceInstance(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      behaviorDefinitionCnxn : Cnxn,
      behaviorDefinitionLabel : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      filters : Seq[CnxnCtxtLabel[String,String,String]],
      onCommencement : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish(
          erql,
          CommenceInstance(
            behaviorDefinitionCnxn,
            behaviorDefinitionLabel,
            cnxns,
            filters
          )
        )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onCommencement( e ) }
      }
    }
    def commenceInstance(
      behaviorDefinitionCnxn : Cnxn,
      behaviorDefinitionLabel : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      filters : Seq[CnxnCtxtLabel[String,String,String]],
      onCommencement : Option[mTT.Resource] => Unit
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      commenceInstance( erql, erspl )(
        behaviorDefinitionCnxn,
        behaviorDefinitionLabel,
        cnxns,
        filters,
        onCommencement
      )
    }
    def completeInstance(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      behaviorInstanceCnxn : Cnxn,
      behaviorInstanceLabel : CnxnCtxtLabel[String,String,String],
      onCompletion : Option[mTT.Resource] => Unit
    ) : Unit = {
      reset {
        node().publish(
          erql,
          CompleteInstance(
            behaviorInstanceCnxn,
            behaviorInstanceLabel
          )
        )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onCompletion( e ) }
      }
    }
    def completeInstance(
      behaviorInstanceCnxn : Cnxn,
      behaviorInstanceLabel : CnxnCtxtLabel[String,String,String],
      onCompletion : Option[mTT.Resource] => Unit
    ) : Unit = {
      val ( erql, erspl ) = makePolarizedPair()
      completeInstance( erql, erspl )(
        behaviorInstanceCnxn,
        behaviorInstanceLabel,
        onCompletion
      )
    }
  }

  @transient
  var _bFactoryMgr : Option[BFactoryManager with Serializable] = None
  def bFactoryMgr( flip : Boolean = false ) : BFactoryManager with Serializable = {
    _bFactoryMgr match {
      case Some( bFactMgr ) => bFactMgr
      case None | null => {    
        node( flip )
        val bFactMgr =
          new BFactoryManager with Serializable {
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

        _bFactoryMgr = Some( bFactMgr )

        bFactMgr
      }
    }    
  }
}

package usage {
  object BFactoryServiceContext
  extends Serializable 
  with UseCaseHelper {    
    @transient
    lazy val eServe =
      new BFactoryCommsService
         with EvalConfig
         with BFactoryCommLinkConfiguration
         with Serializable {
         }    
  }
  object SetupBehaviorMapFromConfigurationFile {
    def makeMap() {
      // For each entry in evalConfig().getMap( "BFactoryMap" ) call BFactoryServiceContext.eserve.mapBehavior
    }
  }
}
