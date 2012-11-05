// -*- mode: Scala;-*- 
// Filename:    ResubmissionTest.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 30 15:51:29 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra.agentservicesstore

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

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

class AgentKVDBNodeResubmitRequestsTest
  extends JUnit4(AgentKVDBNodeResubmitRequestsTestSpecs)

object AgentKVDBNodeResubmitRequestsTestSpecsRunner
  extends ConsoleRunner(AgentKVDBNodeResubmitRequestsTestSpecs)

object AgentKVDBNodeResubmitRequestsTestSpecs extends Specification
with SpecsKVDBHelpers
with RabbitTestSetup
with Timeouts
with Serializable
with FJTaskRunners
{

  //compiler error
//  "AgentKVDBNode" should {
//
//    /* --------------------------------------------------------- *
//     *                       Connections
//     * --------------------------------------------------------- */
//
//    var cnxnRandom =
//      new AgentCnxn(
//	( "ResubmitRequestsTest" + UUID.randomUUID.toString ).toURI,
//	"",
//	( "User" + UUID.randomUUID.toString ).toURI
//      )
//
//    val cnxnUIStore =
//      new AgentCnxn(
//	( "UI" + UUID.randomUUID.toString ).toURI,
//	"",
//	( "Store" + UUID.randomUUID.toString ).toURI
//      )
//
//    val testId = UUID.randomUUID().toString()
//
//    val cnxnTest =
//      new AgentCnxn(
//	( "TestDB" + testId ).toURI,
//	"",
//	( "TestDB" + testId ).toURI
//      )
//
//
//    /* --------------------------------------------------------- *
//     *                           URIs
//     * --------------------------------------------------------- */
//
//    val ui_location =
//      "localhost".toURI.withPort( RABBIT_PORT_UI_PRIVATE )
//    val store_location =
//      "localhost".toURI.withPort( RABBIT_PORT_STORE_PRIVATE )
//    val public_location =
//      "localhost".toURI.withPort( RABBIT_PORT_STORE_PUBLIC )
//
//    /* --------------------------------------------------------- *
//     *                   Additional parameters
//     * --------------------------------------------------------- */
//
//    val numberOfStandingRequests = 10
//
//    /* --------------------------------------------------------- *
//     *                       Scenarios
//     * --------------------------------------------------------- */
//
//    "retrieve between UI and Store with a public queue" in {
//
//      /* --------------------------------------------------------- *
//       *                       KVDBs
//       * --------------------------------------------------------- */
//
//      val uiConfigFileName = None //Some("db_ui.conf")
//      val ui_privateQ =
//	createNode( ui_location, List( store_location ), uiConfigFileName )
//
//      val storeConfigFileName = None //Some("db_store.conf")
//      val store_privateQ =
//	createNode( store_location, List( ui_location ), storeConfigFileName )
//
//      // val msgConfigFileName = Some("db_store.conf")
//      // val store_msgQ = createNode(public_location, List(), msgConfigFileName)
//
//      /* --------------------------------------------------------- *
//       *                      Generated data
//       * --------------------------------------------------------- */
//
//      val contentStrm : Stream[UUID] = uuidRandomStream()
//      val sessionStrm : Stream[UUID] = uuidRandomStream()
//
//      val contentSection = contentStrm.take( numberOfStandingRequests )
//      val sessionSection = sessionStrm.take( numberOfStandingRequests )
//
//      val sessionedContent = sessionSection.zip( contentSection )
//
//      val keyBidsNAsks =
//	for( ( session, content ) <- sessionedContent )
//	yield {
//	  (
//	    (
//	      (
//		"contentRequestPrivate("
//		+ "\"" + session + "\""
//		+ " , "
//		+ "\"" + content + "\""
//		+ ")"
//	      ),
//	      (
//		"test" + session + "@protegra.com"
//	      )
//	    ),
//	    (
//	      "contentRequestPrivate("
//	      + "\"" + session + "\""
//	      + " , "
//	      + "_"
//	      + ")"
//	    )
//	  )
//	}
//
//      val ( keyBids, keyAsks ) = keyBidsNAsks.unzip
//      val ( keyBs, keyVs ) = keyBids.unzip
//
//      val resultKey = "result(\"1\")"
//
//      // spawn {
//      //   reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
//      // }
//
//      /* --------------------------------------------------------- *
//       *                   Store continuations
//       * --------------------------------------------------------- */
//      var barrier : Int = 0
//
//      val keyAskItr = keyAsks.iterator
//
//      while( keyAskItr.hasNext ) { // Delimited continuations don't
//				     // work well with collections, yet
//	val keyPrivate = keyAskItr.next
//
//	spawn {
//	  reset {
//            for ( e <- store_privateQ.get( cnxnUIStore )( keyPrivate.toLabel ) ) {
//	      e match {
//		case Some( _ ) => {
//		  println(
//		    (
//		      "The original behavior for the get on "
//		      + "("
//		      + cnxnUIStore
//		      + " , "
//		      + keyPrivate
//		      + ")"
//		    )
//		  )
//		  val result = e.dispatch
//		  reset {
//		    _resultsQ.put( cnxnTest )( resultKey.toLabel, result )
//		  }
//		}
//		case None => {
//		  println(
//		    (
//		      "listen received - none - on "
//		      + "("
//		      + cnxnUIStore
//		      + " , "
//		      + keyPrivate
//		      + ")"
//		    )
//		  )
//		  barrier += 1
//		}
//	      }
//            }
//	  }
//	}
//      }
//
//      /* --------------------------------------------------------- *
//       * Wait at barrier -- will be fixed with a blocking get, soon
//       * --------------------------------------------------------- */
//
//      while ( barrier < keyAsks.length ) {
//	println( "waiting to get past barrier; current height: " + barrier )
//	Thread.sleep(TIMEOUT_MED)
//      }
//
//      /* --------------------------------------------------------- *
//       *                   Resubmit requests
//       * --------------------------------------------------------- */
//
//      reset {
//	for( pI <- store_privateQ.resubmitRequests( cnxnUIStore )( 1 )) {
//	  spawn {
//	    for( e <- store_privateQ.get( cnxnUIStore )( pI.place ) ) {
//	      e match {
//		case Some( _ ) => {
//		  println(
//		    (
//		      "The new behavior for the get on "
//		      + "("
//		      + cnxnUIStore
//		      + " , "
//		      + pI.place
//		      + ")"
//		    )
//		  )
//		  val result = e.dispatch
//		  reset {
//		    _resultsQ.put( cnxnTest )( resultKey.toLabel, result )
//		  }
//		}
//		case None => {
//		  println(
//		    (
//		      "listen received - none - on "
//		      + "("
//		      + cnxnUIStore
//		      + " , "
//		      + keyPrivate
//		      + ")"
//		    )
//		  )
//		}
//	      }
//	    }
//	  }
//	}
//      }
//
//      /* --------------------------------------------------------- *
//       *                   Assert invariant
//       * --------------------------------------------------------- */
//
//      //fetchString(_resultsQ, cnxnTest, resultKey.toLabel) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//
//  }

}
