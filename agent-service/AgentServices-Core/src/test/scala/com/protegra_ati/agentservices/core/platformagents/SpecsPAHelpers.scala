package com.protegra_ati.agentservices.core.platformagents

import behaviors.Storage
import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._
import com.protegra_ati.agentservices.core.Timeouts
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._

import java.net.URI
import java.util.UUID
import com.protegra_ati.agentservices.core.util.Results

//import com.protegra_ati.agentservices.store.usage.AgentKVDBScope._
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
//import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.mTT._

import actors.threadpool.LinkedBlockingQueue
import com.protegra_ati.agentservices.store.extensions.LinkedBlockingQueueExtensions._
import com.protegra_ati.agentservices.core.util.Results


//import java.net.URI
//import com.biosimilarity.lift.lib.moniker._

trait SpecsPAHelpers extends Timeouts{
  self: SpecificationWithJUnit =>
  //
  //  def getMustBe(expected: String)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
  //  {
  //    println("attempting assert")
  //    println("getMustBe expecting: " + expected)
  //    getData(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
  //
  //    var actual: Data = null
  //
  //    def getData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String):Data = {
  //      q.getData(q._dbQ, cnxn, key, handleGet(_: AgentCnxnProxy, _: Data))
  //      trySleep(actual)
  //      return actual
  //    }
  //
  //    def handleGet(cnxn: AgentCnxnProxy, data: Data) =
  //    {
  //      println("getMustBe - get received : " + data)
  //      actual = data;
  //    }
  //
  //  }


  def fetchMustBe(expected: Data)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
  {
    val resultKey = Results.getKey()
    fetchData(q, cnxn, key, resultKey) must be_== (expected).eventually(5, TIMEOUT_EVENTUALLY)
  }

  def handleFetch(cnxn: AgentCnxnProxy, data: Data, resultKey: String):Unit =
  {
    Results.save(resultKey, data)
  }

  def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String, resultKey: String): Data =
  {
    q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data, resultKey))
    Results.saved(resultKey)
  }

//  def fetchMustBe(expected: Data)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
//  {
//    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String, resultKey: String): Unit =
//    {
//      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data, resultKey))
//    }
//
//    def handleFetch(cnxn: AgentCnxnProxy, data: Data, resultKey: String):Unit =
//    {
//      Results.save(resultKey, data)
//    }
//
//    println("fetchMustBe expecting: " + expected)
//
//    val resultKey = Results.getKey()
//    fetchData(q, cnxn, key, resultKey)
//    Results.saved(resultKey) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
//
//
//  }


  def fetchMustBeWithHandler(expectationCheckHandler: (Data, Data) => Unit)(expected: Data, q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Unit =
  {
    val resultKey = Results.getKey()
    expectationCheckHandler(fetchData(q, cnxn, key, resultKey), expected) // expectationCheckHandler must implement something like this: fetchData(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String, resultKey: String): Data =
    {
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data, resultKey))
      Results.saved(resultKey)
    }

    def handleFetch(cnxn: AgentCnxnProxy, data: Data, resultKey: String) =
    {
      Results.save(resultKey, data)
    }

  }

//  def fetchMustExclusivelyBe(expected: Data)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
//  {
//
//    val resultKey = Results.getKey()
//    fetchData(q, cnxn, key, resultKey)
//    Results.saved(resultKey) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
//
//    println("fetchMustBe expecting: " + expected)
//
//    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String, resultKey: String): Unit =
//    {
//      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data, resultKey))
//    }
//
//    def handleFetch(cnxn: AgentCnxnProxy, data: Data, resultKey: String): Unit =
//    {
//      Results.save(resultKey, data)
//    }
//
//    @volatile var actual: List[ Data ] = Nil
//    //println("attempting assert")
//
//    val expectedList: List[ Data ] = List(expected)
//
//    fetchData(q, cnxn, key) must be_==(expectedList).eventually(5, TIMEOUT_EVENTUALLY)
//    //println("fetchMustBe expecting: " + expected)
//
//    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): List[ Data ] =
//    {
//      //println("BasePlatformAgent is = " + q)
//      q.fetchList[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: List[ Data ]))
//      trySleep(actual)
//      //println("----fetched data:" + actual)
//      return actual
//    }
//
//    def handleFetch(cnxn: AgentCnxnProxy, data: List[ Data ]) =
//    {
//      //println("Number of hits: " + data.size)
//      if ( data == null )
//        actual = Nil
//      else actual = data;
//    }
//  }


  def countMustBe(expected: Int)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
  {
    println("attempting count")
    fetchCount(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

//  def fetchCount(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String): Int =
//  {
//    val resultKey = Results.getKey()
//    val lblSearch = key.toLabel
//    reset {
//      for ( c <- q.fetch(true)(cnxn)(lblSearch) ) {
//        if ( c != None ) {
//          var found = 0
//          for ( e <- c.dispatchCursor ) {
//            found += 1
//            println("*************fetchCount - received : " + e.dispatch.toString)
//          }
//          println("*************fetchCount - size : " + found)
//          Results.count(resultKey, found)
//        }
//      }
//    }
//    Results.counted(resultKey)
//  }

  def fetchCount(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Int =
  {
    val resultKey = Results.getKey()
    q.fetchList[ Any ](q._dbQ, cnxn, key, handleFetchCount(_: AgentCnxnProxy, _: List[ Any ], resultKey))
    return Results.counted(resultKey)
  }

  def handleFetchCount(cnxn: AgentCnxnProxy, data: List[ Any ], resultKey: String) =
  {
    for ( e <- data ) {
      println("*************fetchCount - received : " + e.toString)
    }
    println("*************fetchCount - size : " + data.size)
    Results.count(resultKey, data.size)
  }


}
