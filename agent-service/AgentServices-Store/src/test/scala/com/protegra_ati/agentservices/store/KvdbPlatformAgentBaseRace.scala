package com.protegra_ati.agentservices.store

/* User: jklassen
*/

import org.specs2.mutable._

import org.specs2.runner._
import org.junit.runner._

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


import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._
import util.Results

case class KvdbPlatformAgentBaseRace() extends SpecificationWithJUnit
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

          val resultKey = Results.getKey()
          reset {
            for ( e <- reader.get(cnxn)(key) ) {
              if ( e != None ) {
                val result = e.dispatch
                Results.saveString(resultKey, result)
              }
            }
          }
          reset {writer.put(cnxn)(key, Ground(value))}

          Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
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

         val resultKey = Results.getKey()
         reset {
           for ( e <- reader.fetch(cnxn)(key) ) {
             if ( e != None ) {
               val result = e.dispatch
               Results.saveString(resultKey, result)
             }
           }
         }
         reset {writer.put(cnxn)(key, Ground(value))}

         Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
       }
     }

    //issue 55
    "Fetch/Store" should {

         "retrieve" in {
           val sourceId = UUID.randomUUID
           val targetId = sourceId
           val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

           val testId = UUID.randomUUID().toString()
           val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

           val key = "contentChannel(fetchStoreRetrieve(\"email\"))".toLabel
           val value = "fetchStoreRetrieve@protegra"

           writer.store(cnxn)(key, Ground(value))

           //with 1000 sleep the race condition turns into store/fetch which works without watiers.
//           Thread.sleep(1000)
           fetchString(reader, cnxn, key) must be_==(value).eventually(3, TIMEOUT_EVENTUALLY)
         }
       }
//
//    "Fetch Waiter" should {
//      skipped("just for illustrating the behavior")
//         Thread.sleep(timeoutBetween)
//
//         "work" in {
//           val sourceId = UUID.randomUUID
//           val targetId = sourceId
//           val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
//
//           val testId = UUID.randomUUID().toString()
//           val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//           val key = "contentChannel(fetchWatierRetrieve(\"email\"))".toLabel
//           val value = "fetchWatierRetrieve@protegra"
//
//           SleepToPreventContinuation()
//           fetchString(reader, cnxn, key) must be_==(value).eventually(3, TIMEOUT_EVENTUALLY)
//         }
//       }

  }

}
