package com.protegra_ati.agentservices.store

// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
 
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._



class KvdbPlatformAgentMultipleDistributedTest extends KvdbPlatformAgentBase
{
  sequential

//  val timeoutBetween = TIMEOUT_LONG
  val timeoutBetween = 300

  val writerConfigFileName = Some("db_ui.conf")
  val readerConfigFileName = Some("db_store.conf")

  val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val acquaintanceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)

  val pairedWriter = createNode(sourceAddress, List(acquaintanceAddress), writerConfigFileName)
  val pairedReader = createNode(acquaintanceAddress, List(sourceAddress), readerConfigFileName)

//  testMessaging(pairedWriter, pairedReader)
//  the testWildcardWithPut tests can be intermittent when distributed
//  testWildcardWithPut(pairedWriter, pairedReader)
//  testWildcardWithStore(pairedWriter, pairedReader)
//  testWildcardWithPutAndCursor(pairedWriter, pairedReader)
//  testWildcardWithStoreAndCursor(pairedWriter, pairedReader)
//  testWildcardWithCursorBefore(pairedWriter, pairedReader)

  "read " should {
      "find a results without continuation" in {

        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

        val key = "pub(_)".toLabel

        val key1 = "pub(\"1\")".toLabel
          pairedWriter.store(cnxn)(key1, Ground("1"))
        Thread.sleep(5000)
        reset {
           for ( e <- pairedReader.read(cnxn)(key) ) {
             if ( e != None ) {
               println("Read = " + e.dispatch)
               //        Results.saveString(resultKey, e.dispatch)
             }
           }

         }
        Thread.sleep(1000)
        Thread.sleep(1000)
        Thread.sleep(1000)
        Thread.sleep(1000)
        Thread.sleep(1000)
        success
      }
    }
}

