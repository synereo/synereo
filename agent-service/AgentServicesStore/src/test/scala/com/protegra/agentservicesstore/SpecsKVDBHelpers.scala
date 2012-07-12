package com.protegra.agentservicesstore

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
  
import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import Being.AgentKVDBNodeFactory

import actors.threadpool.LinkedBlockingQueue

import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._

trait SpecsKVDBHelpers
  extends Timeouts
  with RabbitTestSetup
{
  self: Specification =>

  val _resultsQ = createNode("127.0.0.1".toURI.withPort(RABBIT_PORT_TEST_RESULTS_DB), List[ URI ]())


  def createNode(sourceAddress: URI, acquaintanceAddresses: List[ URI ]): Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
  {
    AgentKVDBNodeFactory.ptToMany(sourceAddress, acquaintanceAddresses)
  }

  def getMustBe(expected: String)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    println("getMustBe expecting: " + expected)
    getString(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def getString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    val testId = UUID.randomUUID().toString()
    val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

    reset {
      for ( e <- q.get(cnxn)(key) ) {
        if (e != None)
        {
          reset {_resultsQ.put(cnxnTest)(key, e.dispatch)}
          println("storing to resultQ : " + e.dispatch)
        }
      }
    }

    Thread.sleep(TIMEOUT_MED)
    val actual = fetchString(_resultsQ, cnxnTest, key)
    return actual
  }

  def getMustContain(expected: java.util.List[ String ], resultQ: LinkedBlockingQueue[ String ])(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    getResultQ(resultQ)(q, cnxn, key).containsAll(expected) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def getResultQ(resultQ: LinkedBlockingQueue[ String ])(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): LinkedBlockingQueue[ String ] =
  {
    reset {
      for ( e <- q.get(cnxn)(key) ) {
        if ( e.dispatch.toString != "" ) {
          resultQ.add(e.dispatch)
          println("getResultQ - get received : " + e.toString)
        }
      }
    }
    return resultQ
  }

  def getCount(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String): Int =
  {
    var found = 0
    val lblSearch = key.toLabel
    reset {
      for ( c <- q.get(true)(cnxn)(lblSearch) ) {
        if ( c != None ) {
          for ( e <- c.dispatchCursor ) {
            println("*************getCount - received : " + e.dispatch.toString)
            if ( e.dispatch.toString != "" ) found += 1
          }
        }
      }
    }
    println("-------------returning - found: " + found.toString + " for cnxn " + cnxn.toString)
    trySleep(found)
    return found
  }

  def fetchMustBe(expected: String)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    fetchResultString(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
  }

  def fetchResultString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    val testId = UUID.randomUUID().toString()
    val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

    reset {
      for ( e <- q.fetch(cnxn)(key) ) {
        if (e != None)
        {
          reset {_resultsQ.put(cnxnTest)(key, e.dispatch)}
          println("storing to resultQ : " + e.dispatch)
        }
      }
    }

    Thread.sleep(TIMEOUT_MED)
    val actual = fetchString(_resultsQ, cnxnTest, key)
    return actual
  }

  def fetchString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    var actual = ""
    reset {
      for ( e <- q.fetch(cnxn)(key) ) {
        if ( e != None ) {
          actual = e.dispatch
          println("fetchString received : " + actual)
        }
      }
    }
    return actual
  }

  //  def fetchMustBe(expected: Data)(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: String) =
  //  {
  //    println("attempting assert")
  //    fetchData(q, cnxn, key.toLabel) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  //  }
  //
  //  def fetchData(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse], cnxn: AgentCnxn, key: CnxnCtxtLabel[String, String, String]):Data = {
  //    var actual: Data = null
  //    reset {
  //      for ( e <- q.fetch(cnxn)(key) ) {
  //        if ( e != None ) {
  //          actual = Serializer.deserialize[ Data ](e.dispatch)
  //          val max = if (actual.toString.length() < 255) actual.toString.length() else 255
  //          println("fetchMustbe - fetch received data " + actual.toString.substring(0,max))
  //        }
  //      }
  //    }
  //    return actual
  //  }

  def fetchMustContain(expected: java.util.List[ String ], resultQ: LinkedBlockingQueue[ String ])(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    fetchResultQ(q, cnxn, key).containsAll(expected) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def fetchResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): LinkedBlockingQueue[ String ] =
  {
    var resultQ = new LinkedBlockingQueue[ String ]
    reset {
      for ( e <- q.fetch(cnxn)(key) ) {
        resultQ.add(e.dispatch)
        println("fetchResultQueue - fetch received : " + e.dispatch)
      }
    }
    return resultQ
  }

  def countMustBe(expected: Int)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String) =
  {
    println("attempting count")
    fetchCount(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  //  def fetchCount(q: Being.AgentKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse], cnxn:AgentCnxn, key:String):Int = {
  //    var found = 0
  //    val lblSearch = key.toLabel
  //    reset {
  //      for (e <- q.fetch(cnxn)(lblSearch)) {
  //        if (e != None)
  //        {
  //          found += 1;
  //        }
  //      }
  //    }
  //    println("returning count:" + found)
  //    return found
  //  }

  def fetchCount(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String): Int =
  {
    var found = 0
    val lblSearch = key.toLabel
    reset {
      for ( c <- q.fetch(true)(cnxn)(lblSearch) ) {
        if ( c != None ) {
          for ( e <- c.dispatchCursor ) {
            println("*************fetchCount - received : " + e.dispatch.toString)
            if ( e.dispatch.toString != "" ) found += 1
          }
        }
      }
    }
    println("-------------returning - found: " + found.toString + " for cnxn " + cnxn.toString)
    trySleep(found)
    return found
  }

}