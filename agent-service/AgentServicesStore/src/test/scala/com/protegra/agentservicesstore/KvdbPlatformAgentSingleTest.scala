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

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import Being.AgentKVDBNodeFactory
import actors.threadpool.LinkedBlockingQueue
import com.protegra.agentservicesstore.extensions.LinkedBlockingQueueExtensions._

class KvdbPlatformAgentSingleTest
  extends JUnit4(KvdbPlatformAgentSingleTestSpecs)

object KvdbPlatformAgentSingleTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentSingleTestSpecs)

object KvdbPlatformAgentSingleTestSpecs //extends KvdbPlatformAgentBase
extends Specification
with Timeouts
{
  val timeoutBetween = 0

  val sourceAddress = "127.0.0.1"
  val acquaintanceAddresses = List[URI]()
  val _localQ = AgentKVDBNodeFactory.ptToMany(sourceAddress.toURI, acquaintanceAddresses)
//  val _localQ = createJunction(sourceAddress.toURM, acquaintanceAddresses)
//  def createJunction(sourceAddress: URM, acquaintanceAddresses: List[URM]): PartitionedStringMGJ = new PartitionedStringMGJ(sourceAddress, acquaintanceAddresses, None)


//  testMessaging(_localQ, _localQ)
//  testWildcardWithPut(_localQ, _localQ)
//  testWildcardWithStore(_localQ, _localQ)
//  testWildcardWithCursor(_localQ, _localQ)
//  testWildcardWithCursorBefore(_localQ, _localQ)

  val sourceId = UUID.randomUUID
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

//  "MGJ With Acquaintances" should {
//    "have no implicit error" in {
//      val sourceAddress = "127.0.0.1"
//      val acquaintanceAddresses = List[URM](sourceAddress.toURM)
//      val testQ = new PartitionedStringMGJ(sourceAddress.toURM, acquaintanceAddresses, None)
//      testQ.agentTwistedPairs
//
//      //did not crash by this point
//    }
//  }

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

  //ISSUE 37: different labels get1get2 put1put1 keep going down alternating gets
  "2 Cached Get/Put" should {

    Thread.sleep(timeoutBetween)
    "retrieve" in {
//      val writer = new PartitionedStringMGJ("127.0.0.1".toURM, List[URM](), None)
      //AgentKVDBNodeFactory.ptToMany("127.0.0.1".toURI, List[URI]())
      val Right( ( writerClnt, writerSrvr ) ) = 
	setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( "localhost", 5672, "localhost", 5672 )( true )

      val lblGlobalRequest = "globalRequest(\"email\")".toLabel
      val lblGlobalResponse = "globalResponse(\"email\")".toLabel

      val cnxnGlobal = new AgentCnxn("Global".toURI, "", "Global".toURI)

      println("ready")

      val resultQ = new LinkedBlockingQueue[String]

      def listenGlobalRequest: Unit =
      {
        reset {
          for (e <- writerClnt.get(cnxnGlobal)(lblGlobalRequest)) {
            if (e != None) {
              println("************* REQUEST RECEIVED : " + e.dispatch)
              resultQ.add(e.dispatch)

              listenGlobalRequest
            }
          }
        }
      }

      listenGlobalRequest

      def listenGlobalResponse: Unit =
      {
        reset {
          for (e <- writerClnt.get(cnxnGlobal)(lblGlobalResponse)) {
            if (e != None) {
              println("************* RESPONSE RECEIVED : " + e.dispatch)
              // No message should be received on this label
              fail("Response was received, but should not have been.")

              listenGlobalResponse
            }
          }
        }
      }

      listenGlobalResponse

      val valueGlobalRequest = "START THE GLOBAL REQUEST"
      reset {writerClnt.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 1"))}
      Thread.sleep(TIMEOUT_MED)
      reset {writerClnt.put(cnxnGlobal)(lblGlobalRequest, Ground(valueGlobalRequest + ": 2"))}

      resultQ.size() must be_==(2).eventually(10, TIMEOUT_EVENTUALLY)
    }
  }
}
