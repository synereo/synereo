package com.protegra_ati.agentservices.store

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.BasicLogService
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.util.{Results, Severity}
import org.prolog4j.Solution

import scala.util.continuations._

class PlaceInstanceTest extends KVDBPlatformAgentBase {

  val timeoutBetween        = 0
  val sourceAddress         = "127.0.0.1".toURI
  val acquaintanceAddresses = List[URI]()
  val writer                = createNode(sourceAddress, acquaintanceAddresses)
  val reader                = writer
  val sourceId              = UUID.randomUUID()
  val targetId              = sourceId
  val cnxn                  = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnRandom            = AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

  "prover " should {
    "work" in {
      val prover   = writer.cache.getProver()
      val query    = "pub( X625f0c472fac420293ea3716fcc008bb_ )"
      val label    = "pub( string( 'X1??' ) )"
      val solution = prover.solve(query + " = " + label + ".")
      solution mustBe a[Solution[_]]
    }

    "fail" in {
      val prover = writer.cache.getProver()
      val query  = "pub( X625f0c472fac420293ea3716fcc008bb_ )"
      val label  = "pub( string( 'X1?' ) )"
      an[ArrayIndexOutOfBoundsException] mustBe thrownBy(prover.solve(query + " = " + label + "."))
    }
  }

  "read " should {
    "find a results without continuation" ignore {
      val key       = "pub(_)".toLabel
      val resultKey = Results.getKey()
      val key1      = """pub("1?")""".toLabel
      writer.store(cnxn)(key1, Ground("1"))
      Thread.sleep(1000)
      reset {
        for (e <- writer.read(cnxn)(key)) {
          if (e.isDefined) {
            BasicLogService.tweet(s"Read = ${e.dispatch}")
          }
        }
      }
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      Thread.sleep(1000)
      assert(true)
    }
  }
}
