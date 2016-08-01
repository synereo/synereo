package com.protegra_ati.agentservices.store

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.concurrent._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.Being.AgentKVDBNode
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.{PersistedKVDBNodeRequest, PersistedKVDBNodeResponse}
import com.protegra_ati.agentservices.store.test.{KVDBHelpers, RabbitTestSetup, Timeouts}
import com.protegra_ati.agentservices.store.util.Results
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

import scala.collection.mutable
import scala.util.continuations._

trait PubSubKNodeSetup extends KVDBHelpers with RabbitTestSetup with Timeouts with ThreadPoolRunnersX with Serializable {

  val cnxnUIStore = AgentCnxn(("UI" + UUID.randomUUID.toString).toURI, "", ("Store" + UUID.randomUUID.toString).toURI)
  var cnxnRandom = AgentCnxn(("KUpdateTest" + UUID.randomUUID.toString).toURI, "", ("User" + UUID.randomUUID.toString).toURI)

  val ui_location: URI     = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
  val store_location: URI  = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val public_location: URI = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)

  val testId   = UUID.randomUUID().toString
  val cnxnTest = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)

  val uiConfigFileName: Option[String]    = Some("db_ui.conf")
  val storeConfigFileName: Option[String] = Some("db_store.conf")
  val msgConfigFileName: Option[String]   = Some("db_store.conf")

  val ui_privateQ: AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] =
    createNode(ui_location, List(store_location), uiConfigFileName)

  var store_privateQ: AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] =
    createNode(store_location, List(ui_location), storeConfigFileName)

  //  val store_msgQ = createNode(public_location, List(), msgConfigFileName)

  val keyMsg: String = s"""contentRequestPrivate("${UUID.randomUUID()}")"""

  //  val keyPublic = "contentResponsePublic(_)"
  //  reset {
  //    for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
  //  }

  val resultKey: String  = Results.getKey()
  val keyPrivate: String = "contentRequestPrivate(_)"

  var vBarrier = 0
  var kBarrier = 0

  object OutOfMemoryCounter {

    import com.biosimilarity.lift.lib._

    lazy val srcScope: AMQPStdScope[String] = new AMQPStdScope[String]()
    lazy val srcQM                          = new srcScope.AMQPQueueHostExchangeM[String]("localhost", "TestObservations")
    lazy val srcQ                           = srcQM.zero[String]

    def recordObservation(observation: String): String = {
      report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                 |recording observation: $observation
                 |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
      srcQ ! observation
      observation
    }

    def reportObservations(): Unit = {
      report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                 |reporting observations
                 |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
      val observationMap: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
      for (msg <- srcQM(srcQ)) {
        report(s"observed $msg ${observationMap.getOrElse(msg, 0) + 1} times")
        observationMap += (msg -> (observationMap.getOrElse(msg, 0) + 1))
      }
      for ((k, v) <- observationMap) {
        report("observed " + k + " " + v + " times")
      }
      report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                 |observations report
                 |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    }
  }

  def subscription(): Unit = {
    reset {
      for (e <- store_privateQ.subscribe(cnxnUIStore)(keyPrivate.toLabel)) {
        if (e.isDefined) {
          val result = e.dispatch
          report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                     |subscribe received - $result
                     |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
          OutOfMemoryCounter.recordObservation(result)
          Results.saveString(resultKey, result)
          vBarrier += 1
          //spawn { subscription() }
        } else {
          report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                     |subscribe received - none
                     |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
          kBarrier += 1
        }
      }
    }
  }

  def publication(n: Int, keyMsg: String, value: String): Unit = {
    n match {
      case i: Int if i > 0 =>
        val pval: String = value + UUID.randomUUID.toString
        report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                   |publishing $pval to $keyMsg
                   |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
        reset {
          ui_privateQ.publish(cnxnUIStore)(keyMsg.toLabel, Ground(pval))
        }
        publication(n - 1, keyMsg, value)
      case _ =>
        report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                   |publication complete
                   |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    }
  }

  def testBehavior(value: String, barrierCount: Int, useBarrier: Boolean): Unit = {
    var count = 0
    report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
               | calling subscription
               |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    subscription()
    report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
               | subscription called
               |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    // publication( 5, keyMsg, value )
    if (useBarrier) {
      while ((kBarrier < 1) && (count < barrierCount)) {
        report("waiting to get over kBarrier")
        report("count = " + count)
        count += 1
        Thread.sleep(TIMEOUT_MED)
      }
    }
    // publication( 5, keyMsg, value )
    count = 0
    if (useBarrier) {
      while ((vBarrier < 1) && (count < barrierCount)) {
        report("waiting to get over vBarrier")
        report("count = " + count)
        count += 1
        Thread.sleep(TIMEOUT_MED)
      }
    }
    publication(5, keyMsg, value)
    publication(5, keyMsg, value)
    publication(5, keyMsg, value)
    count = 0
    if (useBarrier) {
      while ((vBarrier < 1) && (count < barrierCount)) {
        report("waiting to get over vBarrier")
        report("count = " + count)
        count += 1
        Thread.sleep(TIMEOUT_MED)
      }
    }
    OutOfMemoryCounter.reportObservations()
  }
}

class AgentKVDBNodeKUpdatePubSubTest
    extends WordSpec
    with MustMatchers
    with Eventually
    with IntegrationPatience
    with RabbitTestSetup
    with Timeouts
    with Serializable {

  "AgentKVDBNode" should {

    "retrieve between UI and Store with a public queue using the migrated continuation" ignore new PubSubKNodeSetup {
      val restoredKey, restored = ""
      testBehavior("test", 50, useBarrier = true)
      Thread.sleep(TIMEOUT_MED)
      eventually { Results.savedString(restoredKey) must ===(restored) }
    }
  }
}
