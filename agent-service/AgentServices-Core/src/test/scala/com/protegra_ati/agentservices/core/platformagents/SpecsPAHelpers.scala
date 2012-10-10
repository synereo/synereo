package com.protegra_ati.agentservices.core.platformagents

import behaviors.Storage
import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.Timeouts
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._

import java.net.URI
import java.util.UUID

//import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
//import com.protegra.agentservicesstore.AgentTS.mTT._

import actors.threadpool.LinkedBlockingQueue
import com.protegra.agentservicesstore.extensions.LinkedBlockingQueueExtensions._

import scala.concurrent.ops._
//import com.biosimilarity.lift.lib.moniker._

trait SpecsPAHelpers extends Timeouts{
  self: Specification =>
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
    var actual: Data = null

    println("attempting assert")

    fetchData(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
    println("fetchMustBe expecting: " + expected)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Data =
    {
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data))
      trySleep(actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxnProxy, data: Data) =
    {
      actual = data;
    }

  }


  def fetchMustBeWithHandler(expectationCheckHandler: (Data, Data) => Unit)(expected: Data, q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Unit =
  {
    var actual: Data = null
    expectationCheckHandler(fetchData(q, cnxn, key), expected) // expectationCheckHandler must implement something like this: fetchData(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Data =
    {
      println("BasePlatformAgent is = " + q)
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: Data))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxnProxy, data: Data) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }

  def fetchMustExclusivelyBe(expected: Data)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
  {
    @volatile var actual: List[ Data ] = Nil
    //println("attempting assert")

    val expectedList: List[ Data ] = List(expected)

    fetchData(q, cnxn, key) must be_==(expectedList).eventually(5, TIMEOUT_EVENTUALLY)
    //println("fetchMustBe expecting: " + expected)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): List[ Data ] =
    {
      //println("BasePlatformAgent is = " + q)
      q.fetchList[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: List[ Data ]))
      trySleep(actual)
      //println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxnProxy, data: List[ Data ]) =
    {
      //println("Number of hits: " + data.size)
      if ( data == null )
        actual = Nil
      else actual = data;
    }
  }


  def countMustBe(expected: Int)(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String) =
  {
    var found = 0

    println("attempting count")
    fetchCount(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)

    def fetchCount(q: BasePlatformAgent with Storage, cnxn: AgentCnxnProxy, key: String): Int =
    {
      q.fetchList[ Any ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxnProxy, _: List[ Any ]))
      trySleep(found)
      return found
    }

    def handleFetch(cnxn: AgentCnxnProxy, data: List[ Any ]) =
    {
      println("getMustBe - get received : " + data)
      for ( e <- data ) {
        println("*************fetchCount - received : " + e.toString)
        if ( e.toString != "" )
          found += 1
      }
    }
  }

}
