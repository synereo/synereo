// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleRaceTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.protegra.agentservicesstore

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

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
import util.Results

class KvdbPlatformAgentSingleRaceTest extends KvdbPlatformAgentBaseRace
{
  val timeoutBetween = 0

  val sourceAddress = "127.0.0.1".toURI
  val acquaintanceAddresses = List[ URI ]()
  val writer = createNode(sourceAddress, acquaintanceAddresses)
  val reader = writer

  testMessaging(writer, reader)

  //example only, must comment out above for it to work , too many nodes
//  "recursive Get 1 Put" should {
//    val sourceId = UUID.randomUUID
//    val targetId = sourceId
//    val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
//    val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
//
//    val cnxnUIStore = new AgentCnxn(( "UI" + sourceId.toString ).toURI, "", ( "Store" + targetId.toString ).toURI)
//
//    val writerConfigFileName = Some("db_ui.conf")
//    val readerConfigFileName = Some("db_store.conf")
//
//    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
//    val acquaintanceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
//
//    val pairedWriter = createNode(sourceAddress, List(acquaintanceAddress), writerConfigFileName)
//    val pairedReader = createNode(acquaintanceAddress, List(sourceAddress), readerConfigFileName)
//
//
//    Thread.sleep(timeoutBetween)
//    "retrieve" in {
//
//      val lblChannel = "contentRequest(_)".toLabel
//
//      val resultKey = Results.getKey()
//      def listenContentRequest(): Unit =
//      {
//        reset {
//          for ( e <- pairedReader.get(cnxnUIStore)(lblChannel) ) {
//            if ( e != None ) {
//              spawn {
//                val result = e.dispatch
//                System.err.println("received " + result)
//                Results.saveString(resultKey, result)
//              }
//              listenContentRequest
//            }
//          }
//        }
//      }
//
//      listenContentRequest
//
//      Thread.sleep(TIMEOUT_MED)
//      val lblContentRequest = ( "contentRequest(\"" + UUID.randomUUID().toString + "\")" ).toLabel
//      val value = "test"
//      reset {pairedWriter.put(cnxnUIStore)(lblContentRequest, Ground(value))}
//
//      Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
//    }
//  }

}
