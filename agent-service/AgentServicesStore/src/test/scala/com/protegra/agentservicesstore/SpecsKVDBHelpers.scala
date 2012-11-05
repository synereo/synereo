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
    AgentKVDBNodeFactory.ptToMany(sourceAddress, acquaintanceAddresses)(configFileName)
  }

  def getMustBe(expected: String)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    println("getMustBe expecting: " + expected)
    getResultString(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
  }

  def getResultString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
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

    SleepToPreventContinuation()
    val actual = fetchString(_resultsQ, cnxnTest, key)
    return actual
  }

  def getString(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): String =
  {
    var actual = ""
    reset {
      for ( e <- q.get(cnxn)(key) ) {
        if ( e != None ) {
          actual = e.dispatch
          println("getString received : " + actual)
        }
      }
    }
    return actual
  }

  def getMustContain(expected: java.util.List[ String ])(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]) =
  {
    println("attempting assert")
    getIntoResultQ(q, cnxn, key).containsAll(expected) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
  }

  def getIntoResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): LinkedBlockingQueue[ String ] =
  {
    val cnxnTest = new AgentCnxn(( "TestDB" + cnxn.src.getHost() ).toURI, "", ( "TestDB" + cnxn.trgt.getHost() ).toURI)

    reset {
      for ( e <- q.get(cnxn)(key) ) {
        val result = e.dispatch
        if ( result.toString  != "" ) {
          val resultKey = "result(\"" +   result.toString  +"\")"
          reset {_resultsQ.put(cnxnTest)(resultKey.toLabel, result)}
          println("getIntoResultQ - storing to resultQ : " + result)
        }
      }
    }
    SleepToPreventContinuation()

    val resultSearch = "result(_)".toLabel
    val actual = fetchResultQ(_resultsQ, cnxnTest, resultSearch)
    return actual
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

    SleepToPreventContinuation()
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
  //    fetchData(q, cnxn, key.toLabel) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
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
    fetchIntoResultQ(q, cnxn, key).containsAll(expected) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
  }

  def fetchIntoResultQ(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: CnxnCtxtLabel[ String, String, String ]): LinkedBlockingQueue[ String ] =
  {
    val cnxnTest = new AgentCnxn(( "TestDB" + cnxn.src.getHost() ).toURI, "", ( "TestDB" + cnxn.trgt.getHost() ).toURI)

    reset {
         for ( e <- q.fetch(cnxn)(key) ) {
           val result = e.dispatch
           if ( result.toString  != "" ) {
             val resultKey = "result(\"" +   result.toString  +"\")"
             reset {_resultsQ.put(cnxnTest)(resultKey.toLabel, result)}
             println("fetchIntoResultQ - storing to resultQ : " + result)
           }
         }
       }
    SleepToPreventContinuation()

    val resultSearch = "result(_)".toLabel
    val actual = fetchResultQ(_resultsQ, cnxnTest, resultSearch)
    return actual
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

  def countMustBe(expected: Int)(q: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, key: String) =
  {
    println("attempting count")
    SleepToPreventContinuation()
    fetchCount(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
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
