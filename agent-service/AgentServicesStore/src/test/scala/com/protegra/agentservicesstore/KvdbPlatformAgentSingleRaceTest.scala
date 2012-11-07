// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleRaceTest.scala
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

import scala.concurrent.ops._

class KvdbPlatformAgentSingleRaceTest
  extends JUnit4(KvdbPlatformAgentSingleRaceTestSpecs)

object KvdbPlatformAgentSingleRaceTestSpecsRunner
  extends ConsoleRunner(KvdbPlatformAgentSingleRaceTestSpecs)

object KvdbPlatformAgentSingleRaceTestSpecs extends KvdbPlatformAgentBaseRace
{
  val timeoutBetween = 0

  val sourceAddress = "127.0.0.1".toURI
  val acquaintanceAddresses = List[ URI ]()
  val writer = createNode(sourceAddress, acquaintanceAddresses)
  val reader = writer

//  testMessaging(writer, reader)

  val sourceId = UUID.randomUUID
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

  "recursive Get 1 Put" should {

    val _resultsQ = createNode("127.0.0.1".toURI.withPort(RABBIT_PORT_TEST_RESULTS_DB), List[ URI ]())
    val testId = UUID.randomUUID().toString()
    val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
    val cnxnUIStore = new AgentCnxn(("UI" + sourceId.toString).toURI, "", ("Store" + targetId.toString).toURI)

    val writerConfigFileName = Some("db_ui.conf")
    val readerConfigFileName = Some("db_store.conf")

    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
    val acquaintanceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

    val pairedWriter = createNode(sourceAddress, List(acquaintanceAddress), writerConfigFileName)
    val pairedReader = createNode(acquaintanceAddress, List(sourceAddress), readerConfigFileName)


    Thread.sleep(timeoutBetween)
    "retrieve" in {

      val lblChannel = "contentRequest(_)".toLabel

      def listenContentRequest(): Unit =
      {
        reset {
          for ( e <- pairedReader.get(cnxnUIStore)(lblChannel) ) {
            if ( e != None ) {
              spawn {
              val lblResult = ( "result(\"" + UUID.randomUUID() + "\")" ).toLabel
              reset {_resultsQ.put(cnxnTest)(lblResult, e.dispatch)}
              }
              listenContentRequest
            }
          }
        }
      }

      val lblContentRequest = ("contentRequest(\"" + UUID.randomUUID().toString + "\")").toLabel
      val valueContentRequest = "START THE GLOBAL REQUEST"
      listenContentRequest

      Thread.sleep(TIMEOUT_MED)
      reset {pairedWriter.put(cnxnUIStore)(lblContentRequest, Ground(valueContentRequest + ": 1"))}

      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      Thread.sleep(TIMEOUT_LONG)
      val strResultSearch = "result(_)"
      countMustBe(1)(_resultsQ, cnxnTest, strResultSearch)
    }
  }

}
