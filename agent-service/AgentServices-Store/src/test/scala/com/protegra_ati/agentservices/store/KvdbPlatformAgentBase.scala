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
import java.util.{concurrent, UUID}

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._


import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._
import actors.threadpool.LinkedBlockingQueue
import org.specs2.specification.Scope
import util.Results

case class KvdbPlatformAgentBase() extends SpecificationWithJUnit
with SpecsKVDBHelpers
with Timeouts
with RabbitTestSetup
{

  trait CnxnSetup extends Scope
  with Serializable
  {
    val sourceId = UUID.randomUUID
    val targetId = sourceId
    val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
    val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
  }

  class WildcardSetup(setupData: (AgentCnxn, String) => Unit) extends Scope
   with Serializable
  {
    var resultQ = new LinkedBlockingQueue[ String ]
    val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
    val value = "tests"
    setupData(cnxnRandom, value)
  }

  //we use this so we can test same/same, local1/local2, remote1/remote2
  //this prevents copy paste and so makes sure the same set of tests is run on each
  def testMessaging(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) =
  {
    val timeoutBetween: Int = 300

    "Persisted Put/Get" should {

      Thread.sleep(timeoutBetween)
      "respect connection" in new CnxnSetup
      {
        val key = "testChannel(cachedPutGetRespectingCnxn(\"email\"))".toLabel
        val value = "cachedPutGetRespectingCnxn@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        //        Thread.sleep(TIMEOUT_MED)
        getMustBe("")(reader, cnxnRandom, key)
      }
//
//      "retrieve " in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutGetRetrieve(\"email\"))".toLabel
//        val value = "cachedPutGetRetrieve@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        //        Thread.sleep(TIMEOUT_MED)
//        getMustBe(value)(reader, cnxn, key)
//      }
//
//      "consume " in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutGetConsume(\"email\"))".toLabel
//        val value = "cachedPutGetConsume@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Thread.sleep(TIMEOUT_MED)
//        getMustBe(value)(reader, cnxn, key)
//        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe("")(reader, cnxn, key)
//      }
//
//      "work with UUID" in new CnxnSetup
//      {
//        val key = ( "testChannel(cachedPutGetUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
//        val value = "cachedPutGetUUID@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        //        Thread.sleep(TIMEOUT_MED)
//
//        getMustBe(value)(reader, cnxn, key)
//      }
//
//      "update" in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutGetUpdate(\"email\"))".toLabel
//        val value = "cachedPutGetUpdate@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value + "1"))}
//        reset {writer.put(cnxn)(key, Ground(value + "2"))}
//        reset {writer.put(cnxn)(key, Ground(value + "3"))}
//
//        Thread.sleep(TIMEOUT_MED)
//        getMustBe(value + "3")(reader, cnxn, key)
//      }
    }
//
//    "Cached Get/Put" should {
//      Thread.sleep(timeoutBetween)
//
//      "retrieve" in new CnxnSetup
//      {
//        val testId = UUID.randomUUID().toString()
//        val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//        val key = "contentChannel(cacheGetPutRetrieve(\"email\"))".toLabel
//        val value = "cacheGetPutRetrieve@protegra"
//
//        val resultKey = Results.getKey()
//        reset {
//          for ( e <- reader.get(cnxn)(key) ) {
//            if ( e != None ) {
//              val result = e.dispatch
//              Results.saveString(resultKey, result)
//            }
//          }
//        }
//        Thread.sleep(TIMEOUT_MED)
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      //RACE: see why it fails intermittently
//      "consume" in new CnxnSetup
//      {
//        val testId = UUID.randomUUID().toString()
//        val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//        val key = "contentChannel(cacheGetPutConsume(\"email\"))".toLabel
//        val value = "cacheGetPutConsume@protegra"
//
//        val resultKey = Results.getKey()
//        reset {
//          for ( e <- reader.get(cnxn)(key) ) {
//            if ( e != None ) {
//              val result = e.dispatch
//              println("cacheGetPutConsume receives a result: " + result)
//              Results.saveString(resultKey, result)
//            }
//          }
//        }
//        //        Thread.sleep(TIMEOUT_LONG)
//        Thread.sleep(TIMEOUT_MED)
//        reset {writer.put(cnxn)(key, Ground(value))}
//        //        SleepToPreventContinuation()
//
//        Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
//        fetchMustBe("")(reader, cnxn, key)
//      }
//    }
//
//
//    "Persisted Put/Fetch" should {
//      Thread.sleep(timeoutBetween)
//      "respect connection" in new CnxnSetup
//      {
//        val key = "testChannel(cachedPutFetchRespectingCnxn(\"email\"))".toLabel
//        val value = "cachedPutFetchRespectingCnxn@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        //            Thread.sleep(TIMEOUT_MED)
//        fetchMustBe("")(reader, cnxnRandom, key)
//      }
//
//      "retrieve " in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutFetchRetrieve(\"email\"))".toLabel
//        val value = "cachedPutFetchRetrieve@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        //            Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "not consume " in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutFetchConsume(\"email\"))".toLabel
//        val value = "cachedPutFetchConsume@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "work with UUID" in new CnxnSetup
//      {
//        val key = ( "testChannel(cachedPutFetchUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
//        val value = "cachedPutFetchUUID@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "update" in new CnxnSetup
//      {
//        val key = "contentChannel(cachedPutFetchUpdate(\"email\"))".toLabel
//        val value = "cachedPutFetchUpdate@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value + "1"))}
//        reset {writer.put(cnxn)(key, Ground(value + "2"))}
//        reset {writer.put(cnxn)(key, Ground(value + "3"))}
//
//        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value + "3")(reader, cnxn, key)
//      }
//    }
//
//
//
//    "Cached Fetch/Put" should {
//      Thread.sleep(timeoutBetween)
//
//           //RACE: see why it fails intermittently
//      "retrieve" in new CnxnSetup
//      {
//        val testId = UUID.randomUUID().toString()
//        val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//        val key = "contentChannel(cacheFetchPutRetrieve(\"email\"))".toLabel
//        val value = "cacheFetchPutRetrieve@protegra"
//
//        val resultKey = Results.getKey()
//        reset {
//          for ( e <- reader.fetch(cnxn)(key) ) {
//            if ( e != None ) {
//              println("!!!!!!!!!!!!!!!!!!!!!!!!!got into fetch")
//              val result = e.dispatch
//              Results.saveString(resultKey, result)
//            }
//          }
//        }
//        Thread.sleep(TIMEOUT_LONG)
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "consume" in new CnxnSetup
//      {
//        skipped("isolate")
//
//        val testId = UUID.randomUUID().toString()
//        val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)
//
//        val key = "contentChannel(cacheFetchPutConsumeByWaiter(\"email\"))".toLabel
//        val value = "cacheFetchPutConsumeByWaiter@protegra"
//
//        val resultKey = Results.getKey()
//        reset {
//          for ( e <- reader.fetch(cnxn)(key) ) {
//            if ( e != None ) {
//              val result = e.dispatch
//              Results.saveString(resultKey, result)
//            }
//          }
//        }
//        println("Sleeping for 300")
//        Thread.sleep(TIMEOUT_MED)
//        reset {writer.put(cnxn)(key, Ground(value))}
//        println("Sleeping again for 300")
//        //         SleepToPreventContinuation()
//
//        Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
//        fetchMustBe("")(reader, cnxn, key)
//      }
//    }
//
//    "Store/Get" should {
//      Thread.sleep(timeoutBetween)
//
//      "respect connection" in new CnxnSetup
//      {
//        val key = "testChannel(storeGetRespectingCnxn(\"email\"))".toLabel
//        val value = "storeGetRespectingCnxn@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        Thread.sleep(TIMEOUT_MED)
//        getMustBe("")(reader, cnxnRandom, key)
//      }
//
//      "retrieve " in new CnxnSetup
//      {
//        val key = "contentChannel(storeGetRetrieve(\"email\"))".toLabel
//        val value = "storeGetRetrieve@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        Thread.sleep(TIMEOUT_MED)
//        getMustBe(value)(reader, cnxn, key)
//      }
//
//      "work with UUID" in new CnxnSetup
//      {
//        val key = ( "testChannel(storeGetUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
//        val value = "storeGetUUID@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        Thread.sleep(TIMEOUT_MED)
//        getMustBe(value)(reader, cnxn, key)
//      }
//
//      //this will probably fail once after deleting all databases
//      //      "retrieve from existing db" in new CnxnSetup {
//      //        val cnxnNew = new AgentCnxn("Test".toURI, "", "NewDB".toURI)
//      //
//      //        val key = "testChannel(storeGetRetrieveExisting(\"name\"))".toLabel
//      //        val value = "storeGetRetrieveExisting@protegra.com"
//      //        writer.store(cnxnNew)(key, Ground(value))
//      //
//      //        Thread.sleep(TIMEOUT_MED)
//      //        getMustBe(value)(reader, cnxnNew, key)
//      //      }
//
//      "update" in new CnxnSetup
//      {
//        val key = "contentChannel(storeGetUpdate(\"email\"))".toLabel
//        val value = "storeGetUpdate@protegra.com"
//
//        writer.store(cnxn)(key, Ground(value + "1"))
//        Thread.sleep(TIMEOUT_MED)
//        writer.store(cnxn)(key, Ground(value + "2"))
//        Thread.sleep(TIMEOUT_MED)
//        writer.store(cnxn)(key, Ground(value + "3"))
//        Thread.sleep(TIMEOUT_MED)
//
//        //wont break but there's actually 3 results in the db
//        getMustBe(value + "3")(reader, cnxn, key)
//      }
//
//      "consume" in new CnxnSetup
//      {
//        val key = "testChannel(persistedStoreGetShouldConsume(\"email\"))".toLabel
//        val value = "persistedStoreGetShouldConsume@protegra.com"
//        writer.store(cnxnRandom)(key, Ground(value))
//
//        Thread.sleep(TIMEOUT_MED)
//        //first get takes it out of db
//        getMustBe(value)(reader, cnxnRandom, key)
//
//        Thread.sleep(TIMEOUT_MED)
//        //second get should not find
//        getMustBe("")(reader, cnxnRandom, key)
//      }
//    }
//
//    "Store/Fetch" should {
//      Thread.sleep(timeoutBetween)
//      "respect connection" in new CnxnSetup
//      {
//        val key = "testChannel(storeFetchRespectingCnxn(\"email\"))".toLabel
//        val value = "storeFetchRespectingCnxn@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        //        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe("")(reader, cnxnRandom, key)
//      }
//
//      "retrieve " in new CnxnSetup
//      {
//        val key = "testChannel(storeFetchRetrieve(\"email\"))".toLabel
//        val value = "storeFetchRetrieve@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        //Thread.sleep(TIMEOUT_MED)
//        //        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "not consume " in new CnxnSetup
//      {
//        val key = "testChannel(storeFetchNotConsume(\"email\"))".toLabel
//        val value = "storeFetchNotConsume@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//
//        //        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "work with UUID" in new CnxnSetup
//      {
//        val key = ( "testChannel(storeFetchUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
//        val value = "storeFetchUUID@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        //Thread.sleep(TIMEOUT_MED)
//        //        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//      "update" in new CnxnSetup
//      {
//        val key = "contentChannel(storeFetchUpdate(\"email\"))".toLabel
//        val value = "storeFetchUpdate@protegra.com"
//        writer.store(cnxn)(key, Ground(value + "1"))
//        Thread.sleep(TIMEOUT_MED)
//        writer.store(cnxn)(key, Ground(value + "2"))
//        Thread.sleep(TIMEOUT_MED)
//        writer.store(cnxn)(key, Ground(value + "3"))
//        Thread.sleep(TIMEOUT_MED)
//
//        //wont break but theres actually 3 results in the db
//        fetchMustBe(value + "3")(reader, cnxn, key)
//      }
//
//      //      java.lang.Exception: matchMap failure profile(string(999), '_, '_, '_) profile(string(999), string(Terry), string(Bunio), string(123456789))
//      "search" in new CnxnSetup
//      {
//        val key = "profile(\"999\",\"Terry\",\"Bunio\",\"123456789\")".toLabel
//        val value = "profile for terry bunio"
//        writer.store(cnxn)(key, Ground(value))
//        //Thread.sleep(TIMEOUT_MED)
//        val searchKey = "profile(\"999\",_,_,_)".toLabel
//        Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, searchKey)
//      }
//    }
//
//    "Put/Delete" should {
//
//      Thread.sleep(timeoutBetween)
//
//      "delete when found" in new CnxnSetup
//      {
//        val key = "contentChannel(putDeleteWhenFound(\"email\"))".toLabel
//        val value = "putDeleteWhenFound@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        writer.delete(cnxn)(key)
//
//        fetchMustBe("")(reader, cnxn, key)
//      }
//
//      "not delete when missing " in new CnxnSetup
//      {
//        val key = "contentChannel(putDeleteWhenMissing(\"email\"))".toLabel
//        val keyMissing = "contentChannel(putDeleteWhenMissing(\"missing\"))".toLabel
//        val value = "putDeleteWhenMissing@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        writer.delete(cnxn)(keyMissing)
//        //        Thread.sleep(TIMEOUT_LONG)
//
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//    }
//
//    "Store/Delete" should {
//
//      Thread.sleep(timeoutBetween)
//
//      "delete when found" in new CnxnSetup
//      {
//        val key = "contentChannel(storeDeleteWhenFound(\"email\"))".toLabel
//        val value = "storeDeleteWhenFound@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        writer.delete(cnxn)(key)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe("")(reader, cnxn, key)
//      }
//
//      "not delete when missing " in new CnxnSetup
//      {
//        val key = "contentChannel(storeDeleteWhenMissing(\"email\"))".toLabel
//        val keyMissing = "contentChannel(storeDeleteWhenMissing(\"missing\"))".toLabel
//        val value = "storeDeleteWhenMissing@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        writer.delete(cnxn)(keyMissing)
//        //        Thread.sleep(TIMEOUT_MED)
//
//        fetchMustBe(value)(reader, cnxn, key)
//      }
//
//    }










//    "Get/Store" should {
//      // get/fetch before store, store doesnt look at waiters
//    }
//
//    "Fetch/Store" should {
//      // get/fetch before store, store doesnt look at waiters
//    }


  }

  def testWildcardWithPut(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) = testWildcard(writer, reader, "put", putWildcardData(_, _, _))

  def testWildcardWithStore(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) = testWildcard(writer, reader, "store", storeWildcardData(_, _, _))

  private def putWildcardData(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, value: String) =
  {
    val key1 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key2 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key3 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key4 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key5 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key6 = "fakeChannel(\"" + UUID.randomUUID.toString + "\")"
    val key7 = "fakeChannel(\"" + UUID.randomUUID.toString + "\")"
    val keyEmail = "surveyChannel(level1(\"email\"), level2(\"name\"))"
    reset {writer.put(cnxn)(key1.toLabel, Ground(value + "1"))}
    reset {writer.put(cnxn)(key2.toLabel, Ground(value + "2"))}
    reset {writer.put(cnxn)(key3.toLabel, Ground(value + "3"))}
    reset {writer.put(cnxn)(key4.toLabel, Ground(value + "4"))}
    reset {writer.put(cnxn)(key5.toLabel, Ground(value + "5"))}
    reset {writer.put(cnxn)(key6.toLabel, Ground(value + "6"))}
    reset {writer.put(cnxn)(key7.toLabel, Ground(value + "7"))}
    reset {writer.put(cnxn)(keyEmail.toLabel, Ground(value))}
    Thread.sleep(TIMEOUT_LONG)
  }

  //with 28 resolved timeouts can be shortened, still needed it appears
  private def storeWildcardData(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxn, value: String) =
  {
    val key1 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key2 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key3 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key4 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key5 = "contentChannel(\"" + UUID.randomUUID.toString + "\")"
    val key6 = "fakeChannel(\"" + UUID.randomUUID.toString + "\")"
    val key7 = "fakeChannel(\"" + UUID.randomUUID.toString + "\")"
    val keyEmail = "surveyChannel(level1(\"email\"), level2(\"name\"))"
    writer.store(cnxn)(key1.toLabel, Ground(value + "1"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key2.toLabel, Ground(value + "2"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key3.toLabel, Ground(value + "3"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key4.toLabel, Ground(value + "4"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key5.toLabel, Ground(value + "5"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key6.toLabel, Ground(value + "6"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(key7.toLabel, Ground(value + "7"))
    //    Thread.sleep(TIMEOUT_SHORT)
    writer.store(cnxn)(keyEmail.toLabel, Ground(value))
    Thread.sleep(TIMEOUT_LONG)
  }

  private def setupExpectedResults(value: String) =
  {
    val expectedResults = List(1, 2, 3, 4, 5) map ( f => value + f.toString )
    val expectedCollection: java.util.List[ String ] = new java.util.ArrayList[ String ]
    for ( p <- expectedResults ) expectedCollection.add(p)
    expectedCollection
  }

  private def testWildcard(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], storageMethod: String, setupData: (Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], AgentCnxn, String) => Unit) =
  {
    val timeoutBetween: Int = 300

    "Wildcard Search after " + storageMethod should {
      Thread.sleep(timeoutBetween)


      "find many values by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
      {
        val expectedCollection = setupExpectedResults(value)
        val lblSearch = "contentChannel(_)".toLabel
        getMustContain(expectedCollection)(reader, cnxnRandom, lblSearch)
      }

//      //TODO: this fails looks like a race ocndition
//      "find many values by underscore Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val expectedCollection = setupExpectedResults(value)
//        val lblSearch = "contentChannel(_)".toLabel
//        getMustContain(expectedCollection)(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a value by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "fail(X)".toLabel
//        getMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a nested value by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "abc(xyz(X))".toLabel
//        getMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      //      this is only valid if WildcardSearchShouldNotFind works
//
//      "find a value by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(level1(X), level2(Y))".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(X, Y)".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by underscore Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(_, _)".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find many values by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val expectedCollection = setupExpectedResults(value)
//        val lblSearch = "contentChannel(X)".toLabel
//        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find many values by underscore Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val expectedCollection = setupExpectedResults(value)
//        val lblSearch = "contentChannel(_)".toLabel
//        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a value by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "fail(X)".toLabel
//        fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a nested value by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "abc(xyz(X))".toLabel
//        fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      //this is only valid if WildcardSearchShouldNotFind works
//      "find a value by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(level1(X), level2(Y))".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(X, Y)".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by underscore Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
//      {
//        val lblSearch = "surveyChannel(_, _)".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
    }
  }

  def testWildcardWithPutAndCursor(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) = testWildcardWithCursor(writer, reader, "put", putWildcardData(_, _, _))

  def testWildcardWithStoreAndCursor(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) = testWildcardWithCursor(writer, reader, "store", storeWildcardData(_, _, _))

  def testWildcardWithCursor(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], storageMethod: String, setupData: (Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], AgentCnxn, String) => Unit) =
  {
    System.err.println("===============testWildcardWithCursor====================")


    "Wildcard Search after store with Cursor" should {
      val timeoutBetween: Int = 300

      //RACE: see why it fails
      "find many results by Get" in new CnxnSetup {
        skipped("see what's wrong with the setup")
        getCountMustBe(5)(reader,  cnxnRandom, "contentChannel(_)")
      }

      "not find a value by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
      {
        System.err.println("===============start of find many results by ====================")
        getCountMustBe(0)(reader, cnxnRandom, "fail(X)")
      }

      "not find a nested value by Get" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
      {
        getCountMustBe(0)(reader, cnxnRandom, "abc(xyz(X))")
      }

      "find many results by Fetch" in new CnxnSetup
      {
        skipped("see what's wrong with the setup")
        countMustBe(5)(reader, cnxnRandom, "contentChannel(_)")
      }

      "not find a value by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
      {
        countMustBe(0)(reader, cnxnRandom, "fail(X)")
      }

      "not find a nested value by Fetch" in new WildcardSetup(setupData(writer, _: AgentCnxn, _: String))
      {
        countMustBe(0)(reader, cnxnRandom, "abc(xyz(X))")
      }
    }
  }

  def testWildcardWithCursorBefore(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) =
  {
    "Wildcard Search before put with Cursor" should {
      val timeoutBetween: Int = 300

      val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
      val value = "tests"

      val testId = UUID.randomUUID().toString()
      val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

      "find many results by Get" in new CnxnSetup
      {
        skipped("bug 54")
        val key = "getCountChannel(_)".toLabel
        val RESULT_PREFIX = "R"
        val resultKey = RESULT_PREFIX + Results.getKey()

        reset {
          for ( c <- reader.get(true)(cnxnRandom)(key) ) {
            if ( c != None ) {
              var found = 0
              for ( e <- c.dispatchCursor ) {
                if ( e != None ) {
                  val result = e.dispatch
                  println("*************getCount - received : " + result.toString)
                  found += 1
                  println("*************getCount - found : " + found)
                }
              }
              println("*************getCount - found : " + found)
//              reset {_resultsQ.put(cnxnTest)(resultKey, found.toString)}
              Results.saveString(resultKey, found.toString)
            }
          }
        }

        putWildcardData(writer, cnxnRandom, value)

//        SleepToPreventContinuation()
//        fetchString(_resultsQ, cnxnTest, resultKey) must be_==("5").eventually(10, TIMEOUT_EVENTUALLY)
        Results.savedString(resultKey,3)must be_==("5").eventually(10, TIMEOUT_EVENTUALLY)
      }

      "find many results by Fetch" in new CnxnSetup
      {
        skipped("bug 54")
        val key = "fetchCountChannel(_)".toLabel
        val RESULT_PREFIX = "R"
        val resultKey = RESULT_PREFIX + Results.getKey()

        reset {
          for ( c <- reader.fetch(true)(cnxnRandom)(key) ) {
            if ( c != None ) {
              var found = 0
              for ( e <- c.dispatchCursor ) {
                if ( e != None ) {
                  val result = e.dispatch
                  println("*************fetchCount - received : " + result.toString)
                  found += 1
                  println("*************fetchCount - found : " + found)
                }
              }
              println("*************fetchCount - found : " + found)
              Results.saveString(resultKey, found.toString)
//              reset {_resultsQ.put(cnxnTest)(resultKey, found.toString)}
            }
          }
        }

        putWildcardData(writer, cnxnRandom, value)
//        SleepToPreventContinuation()
//        fetchString(reader, cnxnTest, resultKey) must be_==("5").eventually(10, TIMEOUT_EVENTUALLY)
        Results.savedString(resultKey,3)must be_==("5").eventually(10, TIMEOUT_EVENTUALLY)
      }
    }
  }
}
