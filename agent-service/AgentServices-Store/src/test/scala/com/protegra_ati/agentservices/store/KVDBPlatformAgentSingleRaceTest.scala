// -*- mode: Scala;-*-
// Filename:    KVDBPlatformAgentSingleRaceTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.BasicLogService
import com.biosimilarity.lift.lib.concurrent.FJTR._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.util.Results

import scala.util.continuations._

class KVDBPlatformAgentSingleRaceTest extends KVDBPlatformAgentBaseRace {

  val timeoutBetween        = 0
  val sourceAddress         = "127.0.0.1".toURI
  val acquaintanceAddresses = List[URI]()
  val writer                = createNode(sourceAddress, acquaintanceAddresses)
  val reader                = writer

  //  testMessaging(writer, reader)

  //  example only, must comment out above for it to work , too many nodes
  "recursive Get 1 Put" should {

    Thread.sleep(timeoutBetween)

    "retrieve" ignore {

      val sourceId             = UUID.randomUUID
      val targetId             = sourceId
      val cnxn                 = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
      val cnxnRandom           = AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
      val cnxnUIStore          = AgentCnxn(("UI" + sourceId.toString).toURI, "", ("Store" + targetId.toString).toURI)
      val writerConfigFileName = Some("db_ui.conf")
      val readerConfigFileName = Some("db_store.conf")
      val sourceAddress        = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
      val acquaintanceAddress  = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
      val pairedWriter         = createNode(sourceAddress, List(acquaintanceAddress), writerConfigFileName)
      val pairedReader         = createNode(acquaintanceAddress, List(sourceAddress), readerConfigFileName)
      val lblChannel           = "contentRequest(_)".toLabel

      val resultKey = Results.getKey()
      def listenContentRequest(): Unit = {
        reset {
          for (e <- pairedReader.get(cnxnUIStore)(lblChannel)) {
            if (e != None) {
              spawn {
                val result = e.dispatch
                BasicLogService.tweet("received " + result)
                Results.saveString(resultKey, result)
              }
              listenContentRequest
            }
          }
        }
      }
      listenContentRequest
      Thread.sleep(TIMEOUT_MED)
      val lblContentRequest = s"""contentRequest("${UUID.randomUUID().toString}")""".toLabel
      val value             = "test"
      reset { pairedWriter.put(cnxnUIStore)(lblContentRequest, Ground(value)) }
      eventually { Results.savedString(resultKey) must ===(value) }
    }
  }

  "20 Gets" should {

    Thread.sleep(timeoutBetween)

    "retrieve" ignore {

      val sourceId             = UUID.randomUUID
      val targetId             = sourceId
      val cnxn                 = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
      val cnxnRandom           = AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
      val cnxnUIStore          = AgentCnxn(("UI" + sourceId.toString).toURI, "", ("Store" + targetId.toString).toURI)
      val writerConfigFileName = Some("db_ui.conf")
      val readerConfigFileName = Some("db_store.conf")
      val sourceAddress        = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
      val acquaintanceAddress  = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
      val pairedWriter         = createNode(sourceAddress, List(acquaintanceAddress), writerConfigFileName)
      val pairedReader         = createNode(acquaintanceAddress, List(sourceAddress), readerConfigFileName)

      @transient val lblChannel = "contentRequest(_)".toLabel
      @transient val resultKey  = Results.getKey()

      def listenContentRequest(): Unit = {
        BasicLogService.tweet("listening")
        reset {
          for (e <- pairedReader.get(cnxnUIStore)(lblChannel)) {
            if (e.isDefined) {
              spawn {
                val result = e.dispatch
                BasicLogService.tweet(s"received $result")
                Results.saveString(resultKey, result)
              }
              listenContentRequest()
            }
          }
        }
      }

      listenContentRequest()

      Thread.sleep(TIMEOUT_MED)
      @transient val value = "test"

      for (i <- 1 to 20) {
        val lblContentRequest = s"""contentRequest("$i")""".toLabel
        reset {
          pairedWriter.put(cnxnUIStore)(lblContentRequest, Ground(value))
        }
      }

      BasicLogService.tweet("starting sleeping")
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      BasicLogService.tweet("I'm sleeping")
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      BasicLogService.tweet("I'm sleeping")
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      assert(true)
    }
  }
}
