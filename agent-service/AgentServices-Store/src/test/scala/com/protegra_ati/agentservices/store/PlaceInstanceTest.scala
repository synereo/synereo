// -*- mode: Scala;-*-
// Filename:    KvdbPlatformAgentSingleTest.scala
// Authors:     lgm
// Creation:    Tue Apr  5 20:51:35 2011
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store


import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._

import util.{Severity, Results}

class PlaceInstanceTest extends KvdbPlatformAgentBase
{
  sequential

  val timeoutBetween = 0

  val sourceAddress = "127.0.0.1".toURI
  val acquaintanceAddresses = List[ URI ]()
  val writer = createNode(sourceAddress, acquaintanceAddresses)
  val reader = writer

  val sourceId = UUID.randomUUID
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

  "prover " should {
      "work" in {
        val prover = writer.cache.getProver()
        val query = "pub( X625f0c472fac420293ea3716fcc008bb_ )"
        val label = "pub( string( 'X1??' ) )"

//        val query = "pub(_)"
//        val label = "pub(\"'\"1?\"'\")"
        println(label)
        try
        {
          prover.solve(query + " = " + label + ".")
        }
        catch {
          case e: Exception => report("Uncaught Exception", e, Severity.Error)
        }
        success
      }

    "fail" in {
      val prover = writer.cache.getProver()
      val query = "pub( X625f0c472fac420293ea3716fcc008bb_ )"
      val label = "pub( string( 'X1?' ) )"

      //        val query = "pub(_)"
      //        val label = "pub(\"'\"1?\"'\")"
      println(label)
      try {
        prover.solve(query + " = " + label + ".")
      }
      catch {
        case e: Exception => report("Uncaught Exception", e, Severity.Error)
      }
      success
    }
  }


  "read " should {
      "find a results without continuation" in {
        skipped("isolate")
        val key = "pub(_)".toLabel

        val resultKey = Results.getKey()

        val key1 = "pub(\"1?\")".toLabel
//        Thread.sleep(1000)
//        Thread.sleep(1000)
         writer.store(cnxn)(key1, Ground("1"))
        Thread.sleep(1000)
        reset {
           for ( e <- writer.read(cnxn)(key) ) {
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
        success
      }
    }

}
