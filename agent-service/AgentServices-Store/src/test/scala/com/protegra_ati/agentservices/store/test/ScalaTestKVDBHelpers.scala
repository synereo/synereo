package com.protegra_ati.agentservices.store.test

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.BasicLogService
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage._
import com.protegra_ati.agentservices.store.util.{MemCache, Reporting, Results}
import org.scalatest.concurrent.Eventually
import org.scalatest.{MustMatchers, WordSpec}

import scala.actors.threadpool.LinkedBlockingQueue
import scala.util.continuations._

trait KVDBHelpers extends Timeouts with RabbitTestSetup with Reporting {

  /* --------------------------------------------------------- *
   *                 Test data stream generation
   * --------------------------------------------------------- */

  def tStream[T](seed: T)(fresh: T => T): Stream[T] = {
    lazy val loopStrm: Stream[T] = List(seed).toStream append (loopStrm map fresh)
    loopStrm
  }

  def uuidRandomStream(): Stream[UUID] = tStream[UUID](UUID.randomUUID)((x: UUID) => UUID.randomUUID)

  def uriStream(seed: URI): Stream[URI] = {
    def fresh(uri: URI): URI = {
      val (scheme, host, port, path) = (uri.getScheme, uri.getHost, uri.getPort, uri.getPath)
      val spath                      = path.split('/')
      val exchange = spath.length match {
        case 2               => spath(1)
        case m: Int if m > 2 => spath(1)
        case n: Int if n < 2 => "defaultAgentExchange_0"
      }
      val sExch = exchange.split('_')
      val (exchRoot, exchCount) = sExch.length match {
        case 2 => (sExch(0), sExch(1).toInt)
        case 1 => (sExch(0), 0)
        case n: Int if n > 2 =>
          (("" /: sExch)(_ + "_" + _), sExch(sExch.length - 1).toInt)
      }
      val nExchCount = exchCount + 1
      new URI(scheme, null, host, port, exchRoot + nExchCount, null, null)
    }
    tStream[URI](seed)(fresh)
  }

  def createNode(sourceAddress: URI,
                 acquaintanceAddresses: List[URI]): Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] =
    createNode(sourceAddress, acquaintanceAddresses, Some("db_store.conf"))

  def createNode(sourceAddress: URI,
                 acquaintanceAddresses: List[URI],
                 configFileName: Option[String]): Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = {
    BasicLogService.tweet(s"""|===================================================
                              |CCCCCCCCCCCCCCC Creating a node CCCCCCCCCCCCCCCCCCC
                              |===================================================
                              |Config file: $configFileName
                              |===================================================""".stripMargin)
    val space = AgentUseCase(configFileName)
    val node  = space.createNode(sourceAddress, acquaintanceAddresses, configFileName)
    node
  }
}

trait ScalaTestKVDBHelpers extends KVDBHelpers { self: WordSpec with MustMatchers with Eventually =>

  def getMustBe(expected: String)(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                                  cnxn: AgentCnxn,
                                  key: CnxnCtxtLabel[String, String, String]) = {
    BasicLogService.tweet("attempting assert")
    BasicLogService.tweet(s"getMustBe expecting: $expected")
    eventually { getString(q, cnxn, key) must ===(expected) }
  }

  def getString(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                cnxn: AgentCnxn,
                key: CnxnCtxtLabel[String, String, String]): String = {
    val resultKey = Results.getKey()
    reset {
      for (e <- q.get(cnxn)(key)) {
        if (e.isDefined) {
          val actual = e.dispatch
          Results.saveString(resultKey, actual)
          BasicLogService.tweet(s"getString received : $actual")
        }
      }
    }
    Results.savedString(resultKey)
  }

  def getMustContain(expected: java.util.List[String])(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                                                       cnxn: AgentCnxn,
                                                       key: CnxnCtxtLabel[String, String, String]) = {
    BasicLogService.tweet("attempting assert")
    val RESULT_PREFIX = "R"
    val resultKey     = RESULT_PREFIX + Results.getKey()
    eventually { getIntoResultQ(q, cnxn, key, resultKey) must ===(expected.size()) }
  }

  def getIntoResultQ(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                     cnxn: AgentCnxn,
                     key: CnxnCtxtLabel[String, String, String],
                     resultKey: String): Int = {
    reset {
      for (e <- q.get(cnxn)(key)) {
        val result = e.dispatch
        if (result.toString != "") {
          report("got a result " + result.toString)
          val results = MemCache.getList[String](resultKey)(Results.client)
          if (results == null || !results.contains(result.toString)) {
            MemCache.addToList[String](resultKey, result.toString)(Results.client)
            ()
          }
        }
      }
    }
    getSearchResultFromCache(resultKey)
  }

  def getSearchResultFromCache(resultKey: String): Int = {
    val results = MemCache.getList[String](resultKey)(Results.client)
    results match {
      case null => 0
      case _    => results.size
    }
  }

  def fetchMustBe(expected: String)(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                                    cnxn: AgentCnxn,
                                    key: CnxnCtxtLabel[String, String, String]) = {
    BasicLogService.tweet("attempting assert")
    eventually { fetchString(q, cnxn, key) must ===(expected) }
  }

  def fetchString(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                  cnxn: AgentCnxn,
                  key: CnxnCtxtLabel[String, String, String]): String = {
    val resultKey = Results.getKey()
    reset {
      for (e <- q.fetch(cnxn)(key)) {
        if (e.isDefined) {
          val actual = e.dispatch
          Results.saveString(resultKey, actual)
          BasicLogService.tweet(s"fetchString received : $actual")
        }
      }
    }
    Results.savedString(resultKey, 3)
  }

  def fetchMustContain(expected: java.util.List[String], resultQ: LinkedBlockingQueue[String])(
      q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
      cnxn: AgentCnxn,
      key: CnxnCtxtLabel[String, String, String]) = {
    BasicLogService.tweet("attempting assert")
    val RESULT_PREFIX = "R"
    val resultKey     = RESULT_PREFIX + Results.getKey()
    eventually { fetchIntoResultQ(q, cnxn, key, resultKey) must ===(expected.size()) }
  }

  def fetchIntoResultQ(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                       cnxn: AgentCnxn,
                       key: CnxnCtxtLabel[String, String, String],
                       resultKey: String): Int = {
    reset {
      for (e <- q.fetch(cnxn)(key)) {
        val result = e.dispatch
        if (result.toString != "") {
          report("got a result " + result.toString)
          val results = MemCache.getList[String](resultKey)(Results.client)
          if (results == null || !results.contains(result.toString)) {
            MemCache.addToList[String](resultKey, result.toString)(Results.client)
            ()
          }
        }
      }
    }
    getSearchResultFromCache(resultKey)
  }

  def fetchResultQ(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
                   cnxn: AgentCnxn,
                   key: CnxnCtxtLabel[String, String, String]): LinkedBlockingQueue[String] = {
    var resultQ = new LinkedBlockingQueue[String]
    reset {
      for (c <- q.fetch(cursor = true)(cnxn)(key)) {
        if (c.isDefined) {
          for (e <- c.dispatchCursor) {
            resultQ.add(e.dispatch)
            BasicLogService.tweet(s"fetchResultQueue - fetch received : ${e.dispatch}")
          }
        }
      }
    }
    resultQ
  }

  def getCountMustBe(
      expected: Int)(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: String) = {
    BasicLogService.tweet("attempting count")
    eventually { getCount(q, cnxn, key) must ===(expected) }
  }

  def getCount(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: String): Int = {
    val resultKey = Results.getKey()
    val lblSearch = key.toLabel
    reset {
      for (c <- q.get(true)(cnxn)(lblSearch)) {
        if (c.isDefined) {
          var found = 0
          for (e <- c.dispatchCursor) {
            found += 1
            BasicLogService.tweet(s"*************getCount - received : ${e.dispatch.toString}")
          }
          BasicLogService.tweet(s"*************getCount - size : $found")
          Results.count(resultKey, found)
        }
      }
    }
    Results.counted(resultKey)
  }

  def countMustBe(
      expected: Int)(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: String) = {
    BasicLogService.tweet("attempting count")
    eventually { fetchCount(q, cnxn, key) must ===(expected) }
  }

  def fetchCount(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: String): Int = {
    val resultKey = Results.getKey()
    val lblSearch = key.toLabel
    reset {
      for (c <- q.fetch(true)(cnxn)(lblSearch)) {
        if (c.isDefined) {
          var found = 0
          for (e <- c.dispatchCursor) {
            found += 1
            BasicLogService.tweet(s"*************fetchCount - received : ${e.dispatch.toString}")
          }
          BasicLogService.tweet(s"*************fetchCount - size : $found")
          Results.count(resultKey, found)
        }
      }
    }
    Results.counted(resultKey)
  }
}
