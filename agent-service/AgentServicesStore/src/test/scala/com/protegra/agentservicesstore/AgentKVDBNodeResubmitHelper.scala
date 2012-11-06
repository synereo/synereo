// -*- mode: Scala;-*- 
// Filename:    AgentKVDBNodeResubmitHelper.scala 
// Authors:     lgm                                                    
// Creation:    Mon Nov  5 11:43:55 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra.agentservicesstore

import com.biosimilarity.lift.model.store._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

import scala.util.continuations._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import Being.AgentKVDBNodeFactory

trait AgentKVDBNodeResubmitRequestsTestConfigurationT
extends KVDBHelpers
with FJTaskRunners {
  def testId : String
  def numberOfStandingRequests : Int
  def uiConfigFileName : Option[String]
  def storeConfigFileName : Option[String]

  /* --------------------------------------------------------- *
   *                       Connections
   * --------------------------------------------------------- */

  def cnxnRandom : AgentCnxn    
  def cnxnUIStore : AgentCnxn   
  def cnxnTest : AgentCnxn
  
  /* --------------------------------------------------------- *
   *                           URIs
   * --------------------------------------------------------- */
  
  def ui_location : URI
  def store_location : URI
  def public_location : URI    
  
  /* --------------------------------------------------------- *
   *                           KVDBs
   * --------------------------------------------------------- */

  def uiPrivateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]
  def storePrivateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]

  /* --------------------------------------------------------- *
   *                      Generated data
   * --------------------------------------------------------- */
  
  @transient
  lazy val contentStrm : Stream[UUID] = uuidRandomStream()
  @transient
  lazy val sessionStrm : Stream[UUID] = uuidRandomStream()

  @transient
  lazy val contentSection = contentStrm.take( numberOfStandingRequests )
  @transient
  lazy val sessionSection = sessionStrm.take( numberOfStandingRequests )

  @transient
  lazy val sessionedContent : Stream[( UUID, UUID )] = sessionSection.zip( contentSection )

  @transient
  lazy val keyBidsNAsks : Stream[( ( String, String ), String )] = {
    for( ( session, content ) <- sessionedContent )
    yield {
      (
	(
	  (
	    "contentRequestPrivate("
	    + "\"" + session + "\"" 
	    + " , "
	    + "\"" + content + "\"" 
	    + ")"
	  ),
	  (
	    "test" + session + "@protegra.com"
	  )
	),
	(
	  "contentRequestPrivate("
	  + "\"" + session + "\"" 
	  + " , "
	  + "_" 
	  + ")"
	)
      )
    }
  }

  @transient
  lazy val keyBidsKeyAsks : ( Stream[( String, String )], Stream[String] ) = {
    keyBidsNAsks.unzip
  }
  
  def resultKey() : String = "result(\"1\")"        
}

abstract class AgentKVDBNodeResubmitRequestsTestConfiguration(
  override val testId : String,
  override val numberOfStandingRequests : Int,
  override val uiConfigFileName : Option[String],
  override val storeConfigFileName : Option[String],
  @transient
  override val cnxnRandom : AgentCnxn,
  @transient
  override val cnxnUIStore : AgentCnxn
) extends AgentKVDBNodeResubmitRequestsTestConfigurationT {
  @transient
  var _barrier : Int = 0
  def barrier() : Int = _barrier
  def barrier( b : Int ) : Unit = {
    _barrier = b
  }

  @transient
  lazy val ui_privateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
    uiPrivateQ
  @transient
  lazy val store_privateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
    storePrivateQ

  type ReqGenerator =
    store_privateQ.HashAgentKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]#Generator[Being.emT.PlaceInstance,Unit,Unit]
  /* --------------------------------------------------------- *
   *                   Store continuations
   * --------------------------------------------------------- */      
  
  case class CKR(
    @transient
    cnxnRdr : AgentCnxn,
    @transient
    cnxnWrtr : AgentCnxn,
    keyRdr : String,
    keyWrtr : String,
    @transient
    rsrc : Option[mTT.Resource]
  )

  def ckrReportPresent( ckr : CKR ) : Unit = {
    println(
      (
	"The original behavior for the get on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
      )
    )
    val result = ckr.rsrc.dispatch
    reset {
      _resultsQ.put( ckr.cnxnWrtr )( ckr.keyWrtr.toLabel, result )
    }
  }

  def ckrReportRepresent( ckr : CKR ) : Unit = {
    println(
      (
	"The new behavior for the get on "
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
      )
    )
    val result = ckr.rsrc.dispatch
    reset {
      _resultsQ.put( ckr.cnxnWrtr )( ckr.keyWrtr.toLabel, result )
    }
  }

  def ckrReportAbsent( ckr : CKR ) : Unit = {
    println(
      (
	"listen received - none - on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
      )
    )
    barrier( barrier() + 1 )
  }

  def ckrReportReabsent( ckr : CKR ) : Unit = {
    println(
      (
	"listen received - none - on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
      )
    )
    barrier( barrier() + 1 )
  }

  def registerContinuation(
    keyRdr : String,
    keyWrtr : String,
    kPresent : CKR => Unit,
    kAbsent : CKR => Unit
  ) : Unit = {
    reset {
      for ( e <- store_privateQ.get( cnxnUIStore )( keyRdr.toLabel ) ) {
	val ckr =
	  CKR( cnxnUIStore, cnxnTest, keyRdr, keyWrtr, e )
	e match {
	  case Some( _ ) => { kPresent( ckr ) }
	  case None => { kAbsent( ckr ) }
	}
      }
    }
  }

  def registerContinuations( keyAsks : Stream[String] )(
    kPresent : CKR => Unit,
    kAbsent : CKR => Unit
  ) : Unit = {    
    val keyAskItr = keyAsks.iterator    
    while( keyAskItr.hasNext ) { // Delimited continuations don't
				 // work well with collections, yet
      
      registerContinuation(
	keyAskItr.next, resultKey, kPresent, kAbsent
      )
    }
  }

  def resubmitPlaceInstance( pIGen : ReqGenerator ) : Unit = {
    reset {
      for( pI <- pIGen ) {      
	registerContinuation(
	  ( pI.place + "" ),
	  resultKey,
	  ckrReportRepresent,
	  ckrReportReabsent
	)
      }
    }
  }
  
  def resubmitRequests( keyAsks : Stream[String] ) : Unit = {
    val keyAskItr2 = keyAsks.iterator
    while( keyAskItr2.hasNext ) {
      val keyAsk = keyAskItr2.next
	val opIGen : Option[ReqGenerator] =
	  store_privateQ.resubmitLabelRequests( cnxnUIStore )( keyAsk.toLabel )(dAT.AGetNum)
	opIGen match {
	  case Some( pIGen ) => {
	    println(
	      (
		"found a pIGenerator on "
		+ "("
		+ cnxnUIStore
		+ " , "
		+ keyAsk
		+ ")"
	      )
	    )
	    resubmitPlaceInstance( pIGen )
	  }
	  case None => {
	    println(
	      (
		"Did not find a pIGenerator on "
		+ "("
		+ cnxnUIStore
		+ " , "
		+ keyAsk
		+ ")"
	      )
	    )
	  }
	}	    
    }
  }
}

case class AgentKVDBNodeResubmitRequestsTestDefaultConfiguration()
extends AgentKVDBNodeResubmitRequestsTestConfiguration(
  UUID.randomUUID().toString(),
  10,
  None,
  None,
  new AgentCnxn(
    ( "ResubmitRequestsTest" + UUID.randomUUID.toString ).toURI,
    "",
    ( "User" + UUID.randomUUID.toString ).toURI
  ),
  new AgentCnxn(
    ( "UI" + UUID.randomUUID.toString ).toURI,
    "",
    ( "Store" + UUID.randomUUID.toString ).toURI
  )  
) {
  @transient
  override val cnxnTest =
    new AgentCnxn(
      ( "TestDB" + testId ).toURI,
      "",
      ( "TestDB" + testId ).toURI
    )
  
  @transient
  override val ui_location = "localhost".toURI.withPort( RABBIT_PORT_UI_PRIVATE )
  @transient
  override val store_location = "localhost".toURI.withPort( RABBIT_PORT_STORE_PRIVATE )
  @transient
  override val public_location = "localhost".toURI.withPort( RABBIT_PORT_STORE_PUBLIC )

  override def uiPrivateQ = createNode( ui_location, List( store_location ), uiConfigFileName )
  override def storePrivateQ = createNode( store_location, List( ui_location ), storeConfigFileName )
}
