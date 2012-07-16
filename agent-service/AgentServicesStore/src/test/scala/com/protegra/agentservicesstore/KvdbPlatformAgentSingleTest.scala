// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
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

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import Being.AgentKVDBNodeFactory

class KvdbPlatformAgentSingleTest
  extends JUnit4(KvdbPlatformAgentSingleTestSpecs)

object KvdbPlatformAgentSingleTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentSingleTestSpecs)

object KvdbPlatformAgentSingleTestSpecs extends KvdbPlatformAgentBase
{
  val timeoutBetween = 0

  val sourceAddress = "127.0.0.1".toURI
  val acquaintanceAddresses = List[ URI ]()
  val writer = createNode(sourceAddress, acquaintanceAddresses)
  val reader = writer

  testMessaging(writer, reader)

//    testWildcardWithPut(writer, reader)
  //  testWildcardWithStore(writer, reader)
  //  testWildcardWithCursor(writer, reader)
  //  testWildcardWithCursorBefore(writer, reader)

  val sourceId = UUID.randomUUID
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

//
//  "Get" should {
//    "not find when key is missing" in {
//      val key = "contentResponse(nonexistingGet(\"not here\"))".toLabel
//      getMustBe("")(reader, cnxn, key)
//    }
//  }
//
//  "Fetch" should {
//    "not find when key is missing" in {
//      val key = "contentResponse(nonexistingFetch(\"not here\"))".toLabel
//      fetchMustBe("")(reader, cnxn, key)
//    }
//  }
//
//  //ISSUE 37: different labels get1get2 put1put1 keep going down alternating gets
//  "2 Cached Get/Put" should {
//
//    val _resultsQ = createNode("127.0.0.1".toURI.withPort(RABBIT_PORT_TEST_RESULTS_DB), List[ URI ]())
//    val testId = UUID.randomUUID().toString()
//    val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//    Thread.sleep(timeoutBetween)
//    "retrieve" in {
//
//      val lblGlobalRequest = "globalRequest(\"email\")".toLabel
//      val lblGlobalResponse = "globalResponse(\"email\")".toLabel
//
//      val globalId = UUID.randomUUID().toString()
//      val cnxnGlobal = new AgentCnxn(( "Global" + globalId ).toURI, "", ( "Global" + globalId ).toURI)
//
//      def listenGlobalRequest(): Unit =
//      {
//        reset {
//          for ( e <- reader.get(cnxnGlobal)(lblGlobalRequest) ) {
//            if ( e != None ) {
//              val lblResult = ( "result(\"" + UUID.randomUUID() + "\")" ).toLabel
//              reset {_resultsQ.put(cnxnTest)(lblResult, e.dispatch)}
//              listenGlobalRequest
//            }
//          }
//        }
//      }
//
//      def listenGlobalResponse: Unit =
//      {
//        reset {
//          for ( e <- reader.get(cnxnGlobal)(lblGlobalResponse) ) {
//            if ( e != None ) {
//              println("************* RESPONSE RECEIVED : " + e.dispatch)
//              // No message should be received on this label
//              fail("Response was received, but should not have been.")
//
//              listenGlobalResponse
//            }
//          }
//        }
//      }
//
//      listenGlobalResponse
//
//      val valueGlobalRequest = "START THE GLOBAL REQUEST"
//      listenGlobalRequest
//      Thread.sleep(TIMEOUT_MED)
//      reset {writer.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 1"))}
//      Thread.sleep(TIMEOUT_MED)
//      reset {writer.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 2"))}
//
//      val strResultSearch = "result(_)"
//      countMustBe(2)(_resultsQ, cnxnTest, strResultSearch)
//    }
//  }

}
