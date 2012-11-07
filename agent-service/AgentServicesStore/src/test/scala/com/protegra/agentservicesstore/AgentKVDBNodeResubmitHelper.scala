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
with FJTaskRunners
with Serializable {
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
  lazy val keyBidsNAsks : ( Stream[( String, String )], Stream[String] ) = {
    (
      for( ( session, content ) <- sessionStrm.zip( contentStrm ) )
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
	  )
	)
      },

      for( session <- sessionStrm ) yield {      
	(
	  "contentRequestPrivate("
	  + "\"" + session + "\"" 
	  + " , "
	  + "_" 
	  + ")"
	)
      }
    )
  }  
  
  def resultKey() : String = "result(\"1\")"        
}

abstract class AgentKVDBNodeResubmitRequestsTestConfiguration(
  override val testId : String,
  override val numberOfStandingRequests : Int,
  @transient
  override val uiConfigFileName : Option[String],
  @transient
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
	"\n########################################################\n"
	+ "The original behavior for the get on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
	+ "\n########################################################\n"
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
	"\n########################################################\n"
	+ "The new behavior for the get on "
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
	+ "\n########################################################\n"
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
	"\n########################################################\n"
	+ "listen received - none - on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
	+ "\n########################################################\n"
      )
    )
    barrier( barrier() + 1 )
  }

  def ckrReportReabsent( ckr : CKR ) : Unit = {
    println(
      (
	"\n########################################################\n"
	+ "listen received - none - on " 
	+ "("
	+ ckr.cnxnRdr
	+ " , "
	+ ckr.keyRdr
	+ ")"
	+ "\n########################################################\n"
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
    println(
      "\n########################################################\n"
      + "registering continuation"
      + "keyRdr : " + keyRdr + "\n"
      + "keyRdr.toLabel : " + keyRdr.toLabel + "\n"
      + "keyWrtr : " + keyWrtr + "\n"
      + "kPresent : " + kPresent + "\n"
      + "kAbsent : " + kAbsent + "\n"
      + "\n########################################################\n"
    )
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

  def registerContinuations( @transient keyAsks : Stream[String] )(
    kPresent : CKR => Unit,
    kAbsent : CKR => Unit
  ) : Unit = {    
    @transient
    val keyAskItr = keyAsks.iterator    

    barrier( 0 )

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
	println(
	  "\n########################################################\n"
	  + "pI : " + pI + "\n"
	  + "pI.place : " + pI.place + "\n"
	  + "pI.stuff : " + pI.stuff + "\n"
	  + "pI.subst : " + pI.subst + "\n"
	  + "\n########################################################\n"
	)
	registerContinuation(
	  ( pI.place.toString.replace( "'", "" ).replace( "string(", "\"" ).replace( "),", "\"," ) ),
	  resultKey,
	  ckrReportRepresent,
	  ckrReportReabsent
	)
      }
    }
  }
  
  def resubmitRequests( @transient keyAsks : Stream[String] ) : Unit = {
    @transient
    val keyAskItr2 = keyAsks.iterator

    barrier( 0 )

    while( keyAskItr2.hasNext ) {
      @transient
      val keyAsk = keyAskItr2.next
	val opIGen : Option[ReqGenerator] =
	  store_privateQ.resubmitLabelRequests( cnxnUIStore )( keyAsk.toLabel )(dAT.AGetNum)
	opIGen match {
	  case Some( pIGen ) => {
	    println(
	      (
		"\n########################################################\n"
		+ "found a pIGenerator on "
		+ "("
		+ cnxnUIStore
		+ " , "
		+ keyAsk
		+ ")"
		+ "\n########################################################\n"
	      )
	    )
	    resubmitPlaceInstance( pIGen )
	  }
	  case None => {
	    println(
	      (
		"\n########################################################\n"
		+ "Did not find a pIGenerator on "
		+ "("
		+ cnxnUIStore
		+ " , "
		+ keyAsk
		+ ")"
		+ "\n########################################################\n"
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
