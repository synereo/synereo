package com.protegra_ati.agentservices.store

import java.util.UUID

import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.test.{RabbitTestSetup, ScalaTestKVDBHelpers, Timeouts}
import com.protegra_ati.agentservices.store.util.Results
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.continuations._

class KVDBPlatformAgentBaseRace
    extends WordSpec
    with MustMatchers
    with Eventually
    with IntegrationPatience
    with ScalaTestKVDBHelpers
    with Timeouts
    with RabbitTestSetup {

  // we use this so we can test same/same, local1/local2, remote1/remote2
  // this prevents copy paste and so makes sure the same set of tests is run on each
  def testMessaging(writer: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                    reader: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]) = {
    val timeoutBetween: Int = 300
    val sourceId            = UUID.randomUUID
    val targetId            = sourceId
    val cnxn                = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
    val cnxnRandom          = AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

    "Cached Get/Put" should {
      Thread.sleep(timeoutBetween)

      "retrieve" in {
        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn     = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

        val testId   = UUID.randomUUID().toString
        val cnxnTest = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)

        val key   = "contentChannel(cacheGetPutRetrieve(\"email\"))".toLabel
        val value = "cacheGetPutRetrieve@protegra"

        val resultKey = Results.getKey()
        reset {
          for (e <- reader.get(cnxn)(key)) {
            if (e.isDefined) {
              val result = e.dispatch
              Results.saveString(resultKey, result)
            }
          }
        }
        reset {
          writer.put(cnxn)(key, Ground(value))
        }

        eventually { Results.savedString(resultKey) must ===(value) }
      }
    }

    "Cached Fetch/Put" should {
      Thread.sleep(timeoutBetween)

      "retrieve" in {
        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn     = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

        val testId   = UUID.randomUUID().toString
        val cnxnTest = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)

        val key   = "contentChannel(cacheFetchPutRetrieve(\"email\"))".toLabel
        val value = "cacheFetchPutRetrieve@protegra"

        val resultKey = Results.getKey()
        reset {
          for (e <- reader.fetch(cnxn)(key)) {
            if (e.isDefined) {
              val result = e.dispatch
              Results.saveString(resultKey, result)
            }
          }
        }
        reset {
          writer.put(cnxn)(key, Ground(value))
        }

        eventually { Results.savedString(resultKey) must ===(value) }
      }
    }

    //issue 55
    "Fetch/Store" should {
      "retrieve" ignore {
        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn     = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
        val testId   = UUID.randomUUID().toString
        val cnxnTest = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)
        val key      = """contentChannel(fetchStoreRetrieve("email"))""".toLabel
        val value    = "fetchStoreRetrieve@protegra"
        writer.store(cnxn)(key, Ground(value))
        // with 1000 sleep the race condition turns into store/fetch which works without watiers.
        // Thread.sleep(1000)
        eventually { fetchString(reader, cnxn, key) must ===(value) }
      }
    }

    "Fetch Waiter" should {
      "work" ignore {
        Thread.sleep(timeoutBetween)
        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn     = AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
        val testId   = UUID.randomUUID().toString()
        val cnxnTest = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)
        val key      = """contentChannel(fetchWatierRetrieve("email"))""".toLabel
        val value    = "fetchWatierRetrieve@protegra"
        SleepToPreventContinuation()
        eventually { fetchString(reader, cnxn, key) must ===(value) }
      }
    }
  }
}
