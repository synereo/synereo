package com.protegra_ati.agentservices.store

import org.specs2.mutable._

import org.specs2.runner._
import org.junit.runner._
import org.specs2.matcher._

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
  
import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._


import actors.threadpool.LinkedBlockingQueue

import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._
import util.{Reporting, MemCache, Results}

trait KVDBHelpers extends Timeouts with RabbitTestSetup with Reporting
{

  /* --------------------------------------------------------- *
   *                 Test data stream generation
   * --------------------------------------------------------- */

  def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
    lazy val loopStrm : Stream[T] =
      ( List( seed ) ).toStream append ( loopStrm map fresh );
    loopStrm
  }

  def uuidRandomStream( ) : Stream[UUID] = {
    tStream[UUID]( UUID.randomUUID )( ( x : UUID ) => UUID.randomUUID )
  }

  def uriStream( seed : URI ) : Stream[URI] = {
    def fresh( uri : URI ) : URI = {
	val ( scheme, host, port, path ) =
	  ( uri.getScheme, uri.getHost, uri.getPort, uri.getPath );
	val spath = path.split( '/' )
	val exchange =
	  spath.length match {
	    case 2 => spath( 1 )
	    case m : Int if m > 2 => spath( 1 )
	    case n : Int if n < 2 => "defaultAgentExchange_0"
	  }
	val sExch = exchange.split( '_' )
	val ( exchRoot, exchCount ) =
	  sExch.length match {
	    case 2 => { ( sExch( 0 ), sExch( 1 ).toInt ) }
	    case 1 => { ( sExch( 0 ), 0 ) }
	    case n : Int if n > 2 => {
	      ( ( "" /: sExch )( _ + "_" + _ ), sExch( sExch.length - 1 ).toInt )
	    }
	  }
	val nExchCount = exchCount + 1

	new URI( scheme, null, host, port, exchRoot + nExchCount, null, null )
      }

    tStream[URI]( seed )( fresh )
  }

  def createNode(sourceAddress: URI, acquaintanceAddresses: List[ URI ]): Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
  {
    createNode(sourceAddress, acquaintanceAddresses, Some("db_store.conf") )
  }
  def createNode(sourceAddress: URI, acquaintanceAddresses: List[ URI ], configFileName: Option[String]): Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
  {
    System.err.println("===================================================" )
    System.err.println("CCCCCCCCCCCCCCC Creating a node CCCCCCCCCCCCCCCCCCC" + configFileName)
    System.err.println("===================================================")
    val space = AgentUseCase(configFileName)
    val node = space.createNode(sourceAddress, acquaintanceAddresses, configFileName)
    node
  }
}

trait SpecsKVDBHelpers
  extends KVDBHelpers
{
  self: SpecificationWithJUnit =>

  def getMustBe(expected: String)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    println("getMustBe expecting: " + expected)
    getString(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def getString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    val resultKey = Results.getKey()
    reset {
      for ( e <- q.get(cnxn)(key) ) {
        if ( e != None ) {
          val actual = e.dispatch
          Results.saveString(resultKey, actual)
          println("getString received : " + actual)
        }
      }
    }
    Results.savedString(resultKey)
  }

  def getMustContain(expected: java.util.List[ String ])(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    val RESULT_PREFIX = "R"
    val resultKey = RESULT_PREFIX + Results.getKey()
    getIntoResultQ(q, cnxn, key, resultKey) must be_==(expected.size()).eventually(10, TIMEOUT_EVENTUALLY)
//    getIntoResultQ(q, cnxn, key).containsAll(expected) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def getIntoResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ],resultKey: String): Int =
  {
//    val cnxnTest = new AgentCnxn(( "TestDB" + cnxn.src.getHost() ).toURI, "", ( "TestDB" + cnxn.trgt.getHost() ).toURI)

    reset {
      for ( e <- q.get(cnxn)(key) ) {
        val result = e.dispatch
        if ( result.toString  != "" ) {
          report("got a result " + result.toString)
          val results = MemCache.getList[ String ](resultKey)(Results.client)
          if ( results == null || !results.contains(result.toString) ) {
            MemCache.addToList[ String ](resultKey, result.toString)(Results.client);
            ()
          }
//          val resultKey = "result(\"" +   result.toString  +"\")"
//          reset {_resultsQ.put(cnxnTest)(resultKey.toLabel, result)}
//          println("getIntoResultQ - storing to resultQ : " + result)
        }
      }
    }
//    SleepToPreventContinuation()
//
//    val resultSearch = "result(_)".toLabel
//    val actual = fetchResultQ(_resultsQ, cnxnTest, resultSearch)
//    return actual
    return getSearchResultFromCache(resultKey)
  }

  def getSearchResultFromCache(resultKey: String): Int = {
    val results = MemCache.getList[ String ](resultKey)(Results.client)
     results match {
       case null => 0
       case _ => results.size
     }
  }

  def fetchMustBe(expected: String)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    fetchString(q, cnxn, key) must be_== (expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def fetchString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    val resultKey = Results.getKey()
    reset {
      for ( e <- q.fetch(cnxn)(key) ) {
        if ( e != None ) {
          val actual = e.dispatch
          Results.saveString(resultKey, actual)
          println("fetchString received : " + actual)
        }
      }
    }
    Results.savedString(resultKey,3)
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
    val RESULT_PREFIX = "R"
    val resultKey = RESULT_PREFIX + Results.getKey()
//    fetchIntoResultQ(q, cnxn, key, resultKey).containsAll(expected) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)
    fetchIntoResultQ(q, cnxn, key, resultKey) must be_==(expected.size()).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def fetchIntoResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ],resultKey: String): Int =
  {
//    val cnxnTest = new AgentCnxn(( "TestDB" + cnxn.src.getHost() ).toURI, "", ( "TestDB" + cnxn.trgt.getHost() ).toURI)

    reset {
         for ( e <- q.fetch(cnxn)(key) ) {
           val result = e.dispatch
           if ( result.toString  != "" ) {
             report("got a result " + result.toString)
             val results = MemCache.getList[ String ](resultKey)(Results.client)
             if ( results == null || !results.contains(result.toString) ) {
               MemCache.addToList[ String ](resultKey, result.toString)(Results.client);
               ()
             }
//
//             val resultKey = "result(\"" +   result.toString  +"\")"
//             reset {_resultsQ.put(cnxnTest)(resultKey.toLabel, result)}
//             println("fetchIntoResultQ - storing to resultQ : " + result)
           }
         }
       }
//    SleepToPreventContinuation()

//    val resultSearch = "result(_)".toLabel
//    val actual = fetchResultQ(_resultsQ, cnxnTest, resultSearch)
//    return actual
    return getSearchResultFromCache(resultKey)
  }

  def fetchResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): LinkedBlockingQueue[ String ] =
  {
    var resultQ = new LinkedBlockingQueue[ String ]
    reset {
      for ( c <- q.fetch(true)(cnxn)(key) ) {
        if ( c != None ) {
          for ( e <- c.dispatchCursor ) {
            resultQ.add(e.dispatch)
            println("fetchResultQueue - fetch received : " + e.dispatch)
          }
        }
      }
    }

    return resultQ
  }

  def getCountMustBe(expected: Int)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String) =
  {
    println("attempting count")
    getCount(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def getCount(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String): Int =
  {
    val resultKey = Results.getKey()
    val lblSearch = key.toLabel
    reset {
      for ( c <- q.get(true)(cnxn)(lblSearch) ) {
        if ( c != None ) {
          var found = 0
          for ( e <- c.dispatchCursor ) {
            found += 1
            println("*************getCount - received : " + e.dispatch.toString)
          }
          println("*************getCount - size : " + found)
          Results.count(resultKey, found)
        }
      }
    }
    Results.counted(resultKey)
  }

  def countMustBe(expected: Int)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String) =
  {
    println("attempting count")
    fetchCount(q, cnxn, key) must be_==(expected).eventually(10, TIMEOUT_EVENTUALLY)
  }

  def fetchCount(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String): Int =
  {
    val resultKey = Results.getKey()
    val lblSearch = key.toLabel
    reset {
      for ( c <- q.fetch(true)(cnxn)(lblSearch) ) {
        if ( c != None ) {
          var found = 0
          for ( e <- c.dispatchCursor ) {
            found += 1
            println("*************fetchCount - received : " + e.dispatch.toString)
          }
          println("*************fetchCount - size : " + found)
          Results.count(resultKey, found)
        }
      }
    }
    Results.counted(resultKey)
  }
}
