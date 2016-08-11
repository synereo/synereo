package com.protegra_ati.agentservices.store

import java.util.UUID

import com.biosimilarity.lift.lib.concurrent._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.test.{KVDBHelpers, RabbitTestSetup, Timeouts}
import com.protegra_ati.agentservices.store.util.Results
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

import scala.collection.mutable
import scala.util.continuations._

trait KNodeSetup extends KVDBHelpers with RabbitTestSetup with Timeouts with ThreadPoolRunnersX with Serializable {

  val cnxnUIStore         = AgentCnxn(s"UI${UUID.randomUUID.toString}".toURI, "", s"Store${UUID.randomUUID}".toURI)
  val ui_location         = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
  val store_location      = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val public_location     = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)
  val testId              = UUID.randomUUID().toString
  val cnxnTest            = AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)
  val uiConfigFileName    = Some("db_ui.conf")
  val storeConfigFileName = Some("db_store.conf")
  val msgConfigFileName   = Some("db_store.conf")
  val ui_privateQ         = createNode(ui_location, List(store_location), uiConfigFileName)
  val store_privateQ      = createNode(store_location, List(ui_location), storeConfigFileName)

  //  val store_msgQ = createNode(public_location, List(), msgConfigFileName)

  val keyMsg = s"""contentRequestPrivate("${UUID.randomUUID()}")"""

  //  val keyPublic = "contentResponsePublic(_)"
  //  reset {
  //    for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
  //  }

  val resultKey = Results.getKey()

  val keyPrivate = "contentRequestPrivate(_)"

  var vBarrier = 0
  var kBarrier = 0

  object OutOfMemoryCounter extends Serializable {

    import com.biosimilarity.lift.lib._

    @transient lazy val srcScope = new AMQPStdScope[String]()
    @transient lazy val srcQM    = new srcScope.AMQPQueueHostExchangeM[String]("localhost", "TestObservations")
    @transient lazy val srcQ     = srcQM.zero[String]

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
        report(s"observed $k $v times")
      }
      report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                 |observations report
                 |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    }
  }

  def getLoop(): Unit = {
    reset {
      for (e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel)) {
        if (e.isDefined) {
          val result = e.dispatch
          report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                     |listen received - $result
                     |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
          OutOfMemoryCounter.recordObservation(result)
          Results.saveString(resultKey, result)
          vBarrier += 1
          spawn { getLoop() }
        } else {
          report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                     |listen received - none
                     |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
          kBarrier += 1
        }
      }
    }
  }

  def putLoop(n: Int, keyMsg: String, value: String): Unit = {
    n match {
      case i: Int if i > 0 =>
        val pval: String = value + UUID.randomUUID.toString
        report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                   |writing $value to $keyMsg
                   |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
        reset {
          ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(pval))
        }
        putLoop(n - 1, keyMsg, value)
      case _ =>
        report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
                   |putLoop complete
                   |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    }
  }

  def testBehavior(value: String, barrierCount: Int, useBarrier: Boolean): Unit = {
    var count = 0
    report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
               | calling getLoop 
               |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    getLoop()
    report(s"""|----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>
               | getLoop called 
               |----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>""".stripMargin)
    putLoop(5, keyMsg, value)
    if (useBarrier) {
      while ((kBarrier < 1) && (count < barrierCount)) {
        report("waiting to get over kBarrier")
        report("count = " + count)
        count += 1
        Thread.sleep(TIMEOUT_MED)
      }
    }
    putLoop(5, keyMsg, value)
    count = 0
    if (useBarrier) {
      while ((vBarrier < 1) && (count < barrierCount)) {
        report("waiting to get over vBarrier")
        report("count = " + count)
        count += 1
        Thread.sleep(TIMEOUT_MED)
      }
    }
    putLoop(5, keyMsg, value)
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

class AgentKVDBNodeKUpdateTest
    extends WordSpec
    with MustMatchers
    with Eventually
    with IntegrationPatience
    with RabbitTestSetup
    with Timeouts
    with Serializable {

  "AgentKVDBNode" should {
    "retrieve between UI and Store with a public queue using the migrated continuation" ignore new KNodeSetup {
      @transient val restoredKey, restored = ""
      testBehavior("test", 50, useBarrier = true)
      Thread.sleep(TIMEOUT_MED)
      eventually { Results.savedString(restoredKey) must ===(restored) }
    }
  }
}
