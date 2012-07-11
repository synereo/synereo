package com.protegra.agentservicesstore

/* User: jklassen
*/

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
import com.protegra.agentservicesstore.usage.AgentUseCase._
import Being.AgentKVDBNodeFactory

import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._

case class KvdbPlatformAgentBaseRace() extends Specification
    with SpecsKVDBHelpers
    with Timeouts
    with RabbitTestSetup
{
  //we use this so we can test same/same, local1/local2, remote1/remote2
  //this prevents copy paste and so makes sure the same set of tests is run on each
  def testMessaging(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) =
  {
    val timeoutBetween: Int = 300
    val sourceId = UUID.randomUUID
    val targetId = sourceId
    val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
    val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

    "Cached Get/Put" should {
       Thread.sleep(timeoutBetween)

       "retrieve" in {
         val sourceId = UUID.randomUUID
         val targetId = sourceId
         val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

         val testId = UUID.randomUUID().toString()
         val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

         val key = "contentChannel(cacheGetPutRetrieve(\"email\"))".toLabel
         val value = "cacheGetPutRetrieve@protegra"

         reset {
           for ( e <- reader.get(cnxn)(key) ) {
             if ( e != None ) {
               val result = e.dispatch
               reset {_resultsQ.put(cnxnTest)(key, result)}
             }
           }
         }
      //      Thread.sleep(TIMEOUT_MED)
         reset {writer.put(cnxn)(key, Ground(value))}
         //      Thread.sleep(TIMEOUT_MED)

         fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
       }
     }

    "Cached Fetch/Put" should {
       Thread.sleep(timeoutBetween)

       "retrieve" in {
         val sourceId = UUID.randomUUID
         val targetId = sourceId
         val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

         val testId = UUID.randomUUID().toString()
         val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

         val key = "contentChannel(cacheFetchPutRetrieve(\"email\"))".toLabel
         val value = "cacheFetchPutRetrieve@protegra"

         reset {
           for ( e <- reader.fetch(cnxn)(key) ) {
             if ( e != None ) {
               val result = e.dispatch
               reset {_resultsQ.put(cnxnTest)(key, result)}
             }
           }
         }
       //      Thread.sleep(TIMEOUT_MED)
         reset {writer.put(cnxn)(key, Ground(value))}
       //      Thread.sleep(TIMEOUT_MED)

         fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
       }
     }

  }

}