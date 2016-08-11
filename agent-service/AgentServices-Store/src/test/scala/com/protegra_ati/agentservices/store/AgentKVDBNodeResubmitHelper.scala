// -*- mode: Scala;-*- 
// Filename:    AgentKVDBNodeResubmitHelper.scala 
// Authors:     lgm                                                    
// Creation:    Mon Nov  5 11:43:55 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.BasicLogService
import com.biosimilarity.lift.lib.concurrent._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.test.KVDBHelpers

import scala.collection.mutable
import scala.util.continuations._

trait AgentKVDBNodeResubmitRequestsTestConfigurationT extends KVDBHelpers with ThreadPoolRunnersX with Serializable {

  @transient
  val resultConfigFileName = Some("db_result.conf")
  lazy val _resultsQ       = createNode("127.0.0.1".toURI.withPort(RABBIT_PORT_TEST_RESULTS_DB), List[URI](), resultConfigFileName)

  def testId: String
  def numberOfStandingRequests: Int
  def uiConfigFileName: Option[String]
  def storeConfigFileName: Option[String]

  /* --------------------------------------------------------- *
   *                       Connections
   * --------------------------------------------------------- */

  def cnxnRandom: AgentCnxn
  def cnxnUIStore: AgentCnxn
  def cnxnTest: AgentCnxn

  /* --------------------------------------------------------- *
   *                           URIs
   * --------------------------------------------------------- */

  def ui_location: URI
  def store_location: URI
  def public_location: URI

  /* --------------------------------------------------------- *
   *                           KVDBs
   * --------------------------------------------------------- */

  def uiPrivateQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]
  def storePrivateQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]

  /* --------------------------------------------------------- *
   *                      Generated data
   * --------------------------------------------------------- */

  @transient
  lazy val contentStrm: Stream[UUID] = uuidRandomStream()
  @transient
  lazy val sessionStrm: Stream[UUID] = uuidRandomStream()

  @transient
  lazy val keyBidsNAsks: (Stream[(String, String)], Stream[String]) = (for ((session, content) <- sessionStrm.zip(contentStrm)) yield {
    (s"""contentRequestPrivate("$session" , "$content")""", "test" + session + "@protegra.com")
  }, for (session <- sessionStrm) yield {
    s"""contentRequestPrivate("$session" , _)"""
  })

  def resultKey(): String = """result("1")"""
}

abstract class AgentKVDBNodeResubmitRequestsTestConfiguration(
    override val testId: String,
    override val numberOfStandingRequests: Int,
    @transient override val uiConfigFileName: Option[String],
    @transient override val storeConfigFileName: Option[String],
    override val cnxnRandom: AgentCnxn,
    override val cnxnUIStore: AgentCnxn,
    @transient val keyMap: mutable.HashMap[String, Int],
    val justPullKRecords: Boolean
) extends AgentKVDBNodeResubmitRequestsTestConfigurationT {

  @transient
  var _barrier: Int = 0
  def barrier(): Int        = _barrier
  def barrier(b: Int): Unit = { _barrier = b }

  @transient
  lazy val ui_privateQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = uiPrivateQ
  @transient
  lazy val store_privateQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = storePrivateQ

  type ReqGenerator =
    store_privateQ.HashAgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]#Generator[Being.emT.PlaceInstance, Unit, Unit]

  /* --------------------------------------------------------- *
   *                   Store continuations
   * --------------------------------------------------------- */

  case class CKR(cnxnRdr: AgentCnxn,
                 cnxnWrtr: AgentCnxn,
                 keyRdr: String,
                 keyWrtr: String,
                 @transient rsrc: Option[mTT.Resource],
                 @transient map: mutable.HashMap[String, Int])

  def ckrReportPresent(ckr: CKR): Unit = {
    BasicLogService.tweet(s"""|
                              |########################################################
                              |The original behavior for the get on (${ckr.cnxnRdr} , ${ckr.keyRdr})
                              |########################################################
                              |""".stripMargin)
    val result = ckr.rsrc.dispatch
    reset {
      _resultsQ.put(ckr.cnxnWrtr)(ckr.keyWrtr.toLabel, result)
    }
  }

  def ckrReportRepresent(ckr: CKR): Unit = {
    BasicLogService.tweet(s"""|
                              |########################################################
                              |The new behavior for the get on (${ckr.cnxnRdr} , ${ckr.keyRdr})
                              |########################################################
                              |""".stripMargin)
    val result = ckr.rsrc.dispatch
    reset {
      _resultsQ.put(ckr.cnxnWrtr)(ckr.keyWrtr.toLabel, result)
    }
  }

  def ckrReportAbsent(ckr: CKR): Unit = {
    BasicLogService.tweet(s"""|
                              |########################################################
                              |listen received - none - on (${ckr.cnxnRdr} , ${ckr.keyRdr})
                              |########################################################
                              |""".stripMargin)
    val k = ckr.keyRdr
    val v = ckr.map.getOrElse(ckr.keyRdr, 0) + 1
    ckr.map += ((k, v))
    barrier(barrier() + 1)
  }

  def ckrReportReabsent(ckr: CKR): Unit = {
    BasicLogService.tweet(s"""|
                              |########################################################
                              |listen received - none - on (${ckr.cnxnRdr} , ${ckr.keyRdr})
                              |########################################################
                              |""".stripMargin)
    val k = ckr.keyRdr
    val v = ckr.map.getOrElse(ckr.keyRdr, 0) + 1
    ckr.map += ((k, v))
    barrier(barrier() + 1)
  }

  def registerContinuation(keyRdr: String,
                           keyWrtr: String,
                           kPresent: CKR => Unit,
                           kAbsent: CKR => Unit,
                           map: mutable.HashMap[String, Int]): Unit = {
    BasicLogService.tweet(s"""|
                              |########################################################
                              |registering continuation
                              |keyRdr : $keyRdr
                              |keyRdr.toLabel : ${keyRdr.toLabel}
                              |keyWrtr : $keyWrtr
                              |kPresent : $kPresent
                              |kAbsent : $kAbsent
                              |
                              |########################################################
                              |""".stripMargin)
    reset {
      for (e <- store_privateQ.get(cnxnUIStore)(keyRdr.toLabel)) {
        val ckr = CKR(cnxnUIStore, cnxnTest, keyRdr, keyWrtr, e, map)
        e match {
          case Some(_) => kPresent(ckr)
          case None    => kAbsent(ckr)
        }
      }
    }
  }

  def registerContinuations(@transient keyAsks: Stream[String])(kPresent: CKR => Unit, kAbsent: CKR => Unit): Unit = {
    @transient
    val keyAskItr = keyAsks.iterator

    barrier(0)

    while (keyAskItr.hasNext) {
      // Delimited continuations don't work well with collections, yet
      registerContinuation(keyAskItr.next, resultKey(), kPresent, kAbsent, keyMap)
    }
  }

  def resubmitPlaceInstance(pIGen: ReqGenerator): Unit = {
    reset {
      for (pI <- pIGen) {
        BasicLogService.tweet(s"""|
                                  |########################################################
                                  |pI : $pI
                                  |pI.place : ${pI.place}
                                  |pI.stuff : ${pI.stuff}
                                  |pI.subst : ${pI.subst}
                                  |
                                  |########################################################
                                  |""".stripMargin)
        registerContinuation(pI.place.toString.replace("'", "").replace("string(", "\"").replace("),", "\" ,"),
                             resultKey(),
                             ckrReportRepresent,
                             ckrReportReabsent,
                             keyMap)
      }
    }
  }

  def resubmitRequests(@transient keyAsks: Stream[String]): Unit = {
    @transient
    val keyAskItr2 = keyAsks.iterator

    barrier(0)

    while (keyAskItr2.hasNext) {
      @transient
      val keyAsk = keyAskItr2.next
      justPullKRecords match {
        case true =>
          BasicLogService.tweet(s"""|
                                    |########################################################
                                    | just pulling krecords for ($cnxnUIStore , $keyAsk)
                                    |########################################################
                                    |""".stripMargin)
          store_privateQ.pullCnxnKRecords(cnxnUIStore)(keyAsk.toLabel)
        case false =>
          BasicLogService.tweet(s"""|
                                    |########################################################
                                    | resubmitting requests for ($cnxnUIStore , $keyAsk)
                                    |########################################################
                                    |""".stripMargin)
          val opIGen: Option[ReqGenerator] = store_privateQ.resubmitLabelRequests(cnxnUIStore)(keyAsk.toLabel)(dAT.AGetNum)
          opIGen match {
            case Some(pIGen) =>
              BasicLogService.tweet(s"""|
                                        |########################################################
                                        |found a pIGenerator on ($cnxnUIStore , $keyAsk)
                                        |########################################################
                                        |""".stripMargin)
              resubmitPlaceInstance(pIGen)
            case None =>
              BasicLogService.tweet(s"""|
                                        |########################################################
                                        |Did not find a pIGenerator on ($cnxnUIStore , $keyAsk)
                                        |########################################################
                                        |""".stripMargin)
          }
      }
    }
  }

  def probeStandingRequests(@transient keyBids: Stream[(String, String)]): Unit = {
    @transient
    val keyBidsItr = keyBids.iterator

    while (keyBidsItr.hasNext) {
      @transient
      val (key, value) = keyBidsItr.next
      reset {
        store_privateQ.put(cnxnUIStore)(key.toLabel, value)
      }
    }
  }
}

case class AgentKVDBNodeResubmitRequestsTestDefaultConfiguration()
    extends AgentKVDBNodeResubmitRequestsTestConfiguration(
      UUID.randomUUID().toString,
      10,
      None,
      None,
      new AgentCnxn(("ResubmitRequestsTest" + UUID.randomUUID.toString).toURI, "", ("User" + UUID.randomUUID.toString).toURI),
      new AgentCnxn(("UI" + UUID.randomUUID.toString).toURI, "", ("Store" + UUID.randomUUID.toString).toURI),
      new mutable.HashMap[String, Int](),
      false) {

  override val cnxnTest = new AgentCnxn(("TestDB" + testId).toURI, "", ("TestDB" + testId).toURI)

  @transient
  override val ui_location = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
  @transient
  override val store_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  @transient
  override val public_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)

  override def uiPrivateQ    = createNode(ui_location, List(store_location), uiConfigFileName)
  override def storePrivateQ = createNode(store_location, List(ui_location), storeConfigFileName)
}
