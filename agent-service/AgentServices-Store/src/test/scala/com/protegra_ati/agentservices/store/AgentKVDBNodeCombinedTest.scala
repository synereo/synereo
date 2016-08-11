package com.protegra_ati.agentservices.store

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.BasicLogService
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.test.{KVDBHelpers, RabbitTestSetup, Timeouts}
import com.protegra_ati.agentservices.store.util.Results
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.continuations._

trait NodeSetup extends KVDBHelpers with RabbitTestSetup with Timeouts with Serializable {

  val cnxnUIStore: AgentKVDBMongoScope.acT.AgentCnxn =
    AgentCnxn(("UI" + UUID.randomUUID.toString).toURI, "", ("Store" + UUID.randomUUID.toString).toURI)
  var cnxnRandom: AgentKVDBMongoScope.acT.AgentCnxn =
    AgentCnxn(("CombinedTest" + UUID.randomUUID.toString).toURI, "", ("User" + UUID.randomUUID.toString).toURI)

  val ui_location: URI     = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
  val store_location: URI  = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val public_location: URI = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)

  val testId: String = UUID.randomUUID().toString()

  val cnxnTest: AgentKVDBMongoScope.acT.AgentCnxn = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)

  val uiConfigFileName: Option[String]    = Some("db_ui.conf")
  val storeConfigFileName: Option[String] = Some("db_store.conf")
  val msgConfigFileName: Option[String]   = Some("db_store.conf")

  val ui_privateQ = createNode(ui_location, List(store_location), uiConfigFileName)

  var store_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)

  //  val store_msgQ = createNode(public_location, List(), msgConfigFileName)

  val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"

  //  val keyPublic = "contentResponsePublic(_)"
  //  reset {
  //    for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
  //  }

  val resultKey = Results.getKey()

  val keyPrivate = "contentRequestPrivate(_)"
  reset {
    for (e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel)) {
      if (e != None) {
        val result = e.dispatch
        Results.saveString(resultKey, result)
      } else {
        BasicLogService.tweet("listen received - none")
      }
    }
  }
}

class AgentKVDBNodeCombinedTest
    extends WordSpec
    with MustMatchers
    with Eventually
    with IntegrationPatience
    with RabbitTestSetup
    with Timeouts
    with Serializable {

  // TODO: they all work individually but not together. need an after spec?
  "AgentKVDBNode" should {

    "retrieve between UI and Store with a public queue" ignore new NodeSetup {

      val value = "test@protegra.com"
      reset {
        ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))
      }

      eventually {
        Results.savedString(resultKey) must ===(value)
      }
    }

    "retrieve between UI and Store with a public queue using the persisted continuation" ignore new NodeSetup {

      Thread.sleep(TIMEOUT_MED)
      store_privateQ = null
      Thread.sleep(TIMEOUT_MED)

      val restored_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)
      Thread.sleep(TIMEOUT_LONG)

      val value = "test"
      Thread.sleep(TIMEOUT_MED)
      reset {
        ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))
      }

      eventually {
        Results.savedString(resultKey) must ===(value)
      }
    }

    "retrieve between UI and Store with a public queue using the migrated continuation" ignore new NodeSetup {

      Thread.sleep(TIMEOUT_MED)
      store_privateQ = null
      Thread.sleep(TIMEOUT_MED)

      val restored_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)
      Thread.sleep(TIMEOUT_LONG)

      val restoredKey = Results.getKey()
      val restored    = "restored"
      reset {
        val generator = restored_privateQ.resubmitGet(cnxnUIStore)(keyPrivate.toLabel).getOrElse(throw new Exception("No generator!"))
        for (placeInstance <- generator) {
          reset {
            for (e <- restored_privateQ.get(cnxnUIStore)(keyPrivate.toLabel)) {
              if (e != None) {
                val result = e.dispatch
                Results.saveString(restoredKey, restored)
              } else {
                BasicLogService.tweet("listen received - none")
              }
            }
          }
        }
      }
      val value = "test"
      Thread.sleep(TIMEOUT_MED)
      reset {
        ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))
      }

      eventually {
        Results.savedString(restoredKey) must ===(restored)
      }
    }
  }
}
