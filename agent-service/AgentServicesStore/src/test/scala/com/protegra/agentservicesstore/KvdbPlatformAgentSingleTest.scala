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
@transient
class KvdbPlatformAgentSingleTest
  extends JUnit4(KvdbPlatformAgentSingleTestSpecs)
@transient
object KvdbPlatformAgentSingleTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentSingleTestSpecs)
@transient
object KvdbPlatformAgentSingleTestSpecs extends KvdbPlatformAgentBase
{
  @transient val timeoutBetween = 0

  @transient val sourceAddress = "127.0.0.1"
  @transient val acquaintanceAddresses = List[ URI ]()
  val writer = createNode(sourceAddress.toURI, acquaintanceAddresses)
  val reader = writer
//  testMessaging(null, null)

//  testMessaging(_localQ, _localQ)
  //  testWildcardWithPut(_localQ, _localQ)
  //  testWildcardWithStore(_localQ, _localQ)
  //  testWildcardWithCursor(_localQ, _localQ)
  //  testWildcardWithCursorBefore(_localQ, _localQ)

  val sourceId = UUID.randomUUID
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

  testMessaging(writer, reader)

//  "Get" should {
//    "not find when key is missing" in {
//      val key = "contentResponse(nonexistingGet(\"not here\"))".toLabel
//      getMustBe("")(_localQ, cnxn, key)
//    }
//  }
//
//  "Fetch" should {
//    "not find when key is missing" in {
//      val key = "contentResponse(nonexistingFetch(\"not here\"))".toLabel
//      fetchMustBe("")(_localQ, cnxn, key)
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
//          for ( e <- _localQ.get(cnxnGlobal)(lblGlobalRequest) ) {
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
//          for ( e <- _localQ.get(cnxnGlobal)(lblGlobalResponse) ) {
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
//      reset {_localQ.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 1"))}
//      Thread.sleep(TIMEOUT_MED)
//      reset {_localQ.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 2"))}
//
//      val strResultSearch = "result(_)"
//      countMustBe(2)(_resultsQ, cnxnTest, strResultSearch)
//    }
//  }
//
//  "Cached Get/Put" should {
//     Thread.sleep(timeoutBetween)
//
//     "retrieve" in {
//       @transient val sourceId = UUID.randomUUID
//       @transient val targetId = sourceId
//       @transient val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
//
//       @transient val testId = UUID.randomUUID().toString()
//       @transient val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//       val key = "contentChannel(cacheGetPut(\"email\"))".toLabel
//       val value = "protegra"
//
//       reset {
//         for ( e <- reader.get(cnxn)(key) ) {
//           if ( e != None ) {
//             val result = e.dispatch
//             reset {_resultsQ.put(cnxnTest)(key, result)}
//           }
//         }
//       }
//       println("Sleeping for 300")
//       Thread.sleep(TIMEOUT_MED)
//       reset {writer.put(cnxn)(key, Ground(value))}
//       println("Sleeping again for 300")
//       Thread.sleep(TIMEOUT_MED)
//
//       fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
//     }
//   }

//
//  "Cached Fetch/Put" should {
//     Thread.sleep(timeoutBetween)
//
//     "retrieve" in {
//       @transient val sourceId = UUID.randomUUID
//       @transient val targetId = sourceId
//       @transient val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
//
//       @transient val testId = UUID.randomUUID().toString()
//       @transient val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//       val key = "contentChannel(cacheFetchPut(\"email\"))".toLabel
//       val value = "protegra"
//
//       reset {
//         for ( e <- reader.fetch(cnxn)(key) ) {
//           if ( e != None ) {
//             val result = e.dispatch
//             reset {_resultsQ.put(cnxnTest)(key, result)}
//           }
//         }
//       }
//       println("Sleeping for 300")
//       Thread.sleep(TIMEOUT_MED)
//       reset {writer.put(cnxn)(key, Ground(value))}
//       println("Sleeping again for 300")
//       Thread.sleep(TIMEOUT_MED)
//
//       fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(3, TIMEOUT_EVENTUALLY)
//     }
//   }
}
