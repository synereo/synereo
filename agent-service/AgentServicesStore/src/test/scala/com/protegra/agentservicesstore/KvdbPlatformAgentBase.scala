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
import java.util.{concurrent, UUID}

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._
import Being.AgentKVDBNodeFactory

import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._
import actors.threadpool.LinkedBlockingQueue

case class KvdbPlatformAgentBase() extends Specification
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

    "Persisted Put/Get" should {

      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(cachedPutGetRespectingCnxn(\"email\"))".toLabel
        val value = "cachedPutGetRespectingCnxn@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        getMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(cachedPutGetRetrieve(\"email\"))".toLabel
        val value = "cachedPutGetRetrieve@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        getMustBe(value)(reader, cnxn, key)
      }

      "consume " in {
        val key = "contentChannel(cachedPutGetConsume(\"email\"))".toLabel
        val value = "cachedPutGetConsume@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        getMustBe(value)(reader, cnxn, key)
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe("")(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ( "testChannel(cachedPutGetUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
        val value = "cachedPutGetUUID@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)

        getMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(cachedPutGetUpdate(\"email\"))".toLabel
        val value = "cachedPutGetUpdate@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value + "1"))}
        reset {writer.put(cnxn)(key, Ground(value + "2"))}
        reset {writer.put(cnxn)(key, Ground(value + "3"))}

        Thread.sleep(TIMEOUT_MED)
        getMustBe(value + "3")(writer, cnxn, key)
      }
    }

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
         Thread.sleep(TIMEOUT_MED)
         reset {writer.put(cnxn)(key, Ground(value))}
         Thread.sleep(TIMEOUT_MED)

         fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
       }

      "consume" in {
        val sourceId = UUID.randomUUID
        val targetId = sourceId
        val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

        val testId = UUID.randomUUID().toString()
        val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

        val key = "contentChannel(cacheGetPutConsume(\"email\"))".toLabel
        val value = "cacheGetPutConsume@protegra"

        reset {
          for ( e <- reader.get(cnxn)(key) ) {
            if ( e != None ) {
              val result = e.dispatch
              reset {_resultsQ.put(cnxnTest)(key, result)}
            }
          }
        }
        println("Sleeping for 300")
        Thread.sleep(TIMEOUT_MED)
        reset {writer.put(cnxn)(key, Ground(value))}
        println("Sleeping again for 300")
        Thread.sleep(TIMEOUT_MED)

        fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
        fetchMustBe("")(reader, cnxn, key)
      }
     }

    "Persisted Put/Fetch" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(cachedPutFetchRespectingCnxn(\"email\"))".toLabel
        val value = "cachedPutFetchRespectingCnxn@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(cachedPutFetchRetrieve(\"email\"))".toLabel
        val value = "cachedPutFetchRetrieve@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ( "testChannel(cachedPutFetchUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
        val value = "cachedPutFetchUUID@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(cachedPutFetchUpdate(\"email\"))".toLabel
        val value = "cachedPutFetchUpdate@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value + "1"))}
        reset {writer.put(cnxn)(key, Ground(value + "2"))}
        reset {writer.put(cnxn)(key, Ground(value + "3"))}

        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value + "3")(writer, cnxn, key)
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
         println("Sleeping for 300")
         Thread.sleep(TIMEOUT_MED)
         reset {writer.put(cnxn)(key, Ground(value))}
         println("Sleeping again for 300")
         Thread.sleep(TIMEOUT_MED)

         fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
       }

       "consume" in {
         val sourceId = UUID.randomUUID
         val targetId = sourceId
         val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

         val testId = UUID.randomUUID().toString()
         val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

         val key = "contentChannel(cacheFetchPutConsumeByWaiter(\"email\"))".toLabel
         val value = "cacheFetchPutConsumeByWaiter@protegra"

         reset {
           for ( e <- reader.fetch(cnxn)(key) ) {
             if ( e != None ) {
               val result = e.dispatch
               reset {_resultsQ.put(cnxnTest)(key, result)}
             }
           }
         }
         println("Sleeping for 300")
         Thread.sleep(TIMEOUT_MED)
         reset {writer.put(cnxn)(key, Ground(value))}
         println("Sleeping again for 300")
         Thread.sleep(TIMEOUT_MED)

         fetchString(_resultsQ, cnxnTest, key) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
         fetchMustBe("")(reader, cnxn, key)
       }
     }

    "Store/Get" should {
      Thread.sleep(timeoutBetween)

      "respect connection" in {
        val key = "testChannel(storeGetRespectingCnxn(\"email\"))".toLabel
        val value = "storeGetRespectingCnxn@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        Thread.sleep(TIMEOUT_MED)
        getMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(storeGetRetrieve(\"email\"))".toLabel
        val value = "storeGetRetrieve@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_LONG)
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ( "testChannel(storeGetUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
        val value = "storeGetUUID@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_LONG)
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      //this will probably fail once after deleting all databases
      "retrieve from existing db" in {
        val cnxnNew = new AgentCnxn("Test".toURI, "", "NewDB".toURI)

        val key = "testChannel(storeGetRetrieveExisting(\"name\"))".toLabel
        val value = "storeGetRetrieveExisting@protegra.com"
        writer.store(cnxnNew)(key, Ground(value))

        Thread.sleep(TIMEOUT_MED)
        getMustBe(value)(reader, cnxnNew, key)
      }

      "update" in {
        val key = "contentChannel(storeGetUpdate(\"email\"))".toLabel
        val value = "storeGetUpdate@protegra.com"

        writer.store(cnxn)(key, Ground(value + "1"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "2"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "3"))
        Thread.sleep(TIMEOUT_MED)

        //wont break but theres actually 3 results in the db
        getMustBe(value + "3")(reader, cnxn, key)
      }

      "consume" in {
        val cnxnRandom = new AgentCnxn("Test".toURI, "", UUID.randomUUID.toString.toURI)

        val key = "testChannel(persistedStoreGetShouldConsume(\"email\"))".toLabel
        val value = "persistedStoreGetShouldConsume@protegra.com"
        writer.store(cnxnRandom)(key, Ground(value))

        Thread.sleep(TIMEOUT_MED)
        //first get takes it out of db
        getMustBe(value)(reader, cnxnRandom, key)

        Thread.sleep(TIMEOUT_MED)
        //second get should not find
        getMustBe("")(reader, cnxnRandom, key)
      }
    }

    "Store/Fetch" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(storeFetchRespectingCnxn(\"email\"))".toLabel
        val value = "storeFetchRespectingCnxn@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "testChannel(storeFetchRetrieve(\"email\"))".toLabel
        val value = "storeFetchRetrieve@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_MED)
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "not consume " in {
        val key = "testChannel(storeFetchNotConsume(\"email\"))".toLabel
        val value = "storeFetchNotConsume@protegra.com"
        writer.store(cnxn)(key, Ground(value))

        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
        fetchMustBe(value)(reader, cnxn, key)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ( "testChannel(storeFetchUUID(\"" + UUID.randomUUID.toString + "\"))" ).toLabel
        val value = "storeFetchUUID@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_MED)
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(storeFetchUpdate(\"email\"))".toLabel
        val value = "storeFetchUpdate@protegra.com"
        writer.store(cnxn)(key, Ground(value + "1"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "2"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "3"))
        Thread.sleep(TIMEOUT_MED)

        //wont break but theres actually 3 results in the db
        fetchMustBe(value + "3")(reader, cnxn, key)
      }

//      java.lang.Exception: matchMap failure profile(string(999), '_, '_, '_) profile(string(999), string(Terry), string(Bunio), string(123456789))
      "search" in {
        val key = "profile(\"999\",\"Terry\",\"Bunio\",\"123456789\")".toLabel
        val value = "profile for terry bunio"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_MED)
        val searchKey = "profile(\"999\",_,_,_)".toLabel
        Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, searchKey)
      }
    }

    "Put/Delete" should {

      Thread.sleep(timeoutBetween)

      "delete when found" in {
        val key = "contentChannel(putDeleteWhenFound(\"email\"))".toLabel
        val value = "putDeleteWhenFound@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
        Thread.sleep(TIMEOUT_MED)

        writer.delete(cnxn)(key)

        fetchMustBe("")(reader, cnxn, key)
      }

      "not delete when missing " in {
        val key = "contentChannel(putDeleteWhenMissing(\"email\"))".toLabel
        val keyMissing = "contentChannel(putDeleteWhenMissing(\"missing\"))".toLabel
        val value = "putDeleteWhenMissing@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
        Thread.sleep(TIMEOUT_MED)

        writer.delete(cnxn)(keyMissing)
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
      }

    }

    "Store/Delete" should {

      Thread.sleep(timeoutBetween)

      "delete when found" in {
        val key = "contentChannel(storeDeleteWhenFound(\"email\"))".toLabel
        val value = "storeDeleteWhenFound@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
        Thread.sleep(TIMEOUT_MED)

        writer.delete(cnxn)(key)
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe("")(reader, cnxn, key)
      }

      "not delete when missing " in {
        val key = "contentChannel(storeDeleteWhenMissing(\"email\"))".toLabel
        val keyMissing = "contentChannel(storeDeleteWhenMissing(\"missing\"))".toLabel
        val value = "storeDeleteWhenMissing@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
        Thread.sleep(TIMEOUT_MED)

        writer.delete(cnxn)(keyMissing)
        Thread.sleep(TIMEOUT_MED)

        fetchMustBe(value)(reader, cnxn, key)
      }

    }

//    "Get/Store" should {
//      // get/fetch before store, store doesnt look at waiters
//    }
//
//    "Fetch/Store" should {
//      // get/fetch before store, store doesnt look at waiters
//    }
//

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
    val value = "tests@protegra.com"
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

      var resultQ = new LinkedBlockingQueue[ String ]
      val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
      val value = "tests@protegra.com"
      setupData(writer, cnxnRandom, value)
      val expectedCollection = setupExpectedResults(value)

      "find many results by Get" in {
//        skip("needs cursor fix")
        println("attempting assert")
        getCount(reader, cnxnRandom, "contentChannel(_)") must be_==(5).eventually(3, TIMEOUT_EVENTUALLY)
      }

//      "find many values by Get" in {
//        val lblSearch = "contentChannel(X)".toLabel
//        getMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find many values by underscore Get" in {
//        val lblSearch = "contentChannel(_)".toLabel
//        getMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a value by Get" in {
//        val lblSearch = "fail(X)".toLabel
//        getMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a nested value by Get" in {
//        val lblSearch = "abc(xyz(X))".toLabel
//        getMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
////      this is only valid if WildcardSearchShouldNotFind works
//
//      "find a value by Get" in {
//        val lblSearch = "surveyChannel(level1(X), level2(Y))".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by Get" in {
//        val lblSearch = "surveyChannel(X, Y)".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by underscore Get" in {
//        val lblSearch = "surveyChannel(_, _)".toLabel
//        getMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find many results by Fetch" in {
//        skip("needs cursor fix")
//        println("attempting assert")
//        fetchCount(reader, cnxnRandom, "contentChannel(_)") must be_==(5).eventually(3, TIMEOUT_EVENTUALLY)
//      }
//
//      "find many values by Fetch" in {
//        val lblSearch = "contentChannel(X)".toLabel
//        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find many values by underscore Fetch" in {
//        val lblSearch = "contentChannel(_)".toLabel
//        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a value by Fetch" in {
//        val lblSearch = "fail(X)".toLabel
//        fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      "not find a nested value by Fetch" in {
//        val lblSearch = "abc(xyz(X))".toLabel
//        fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      }
//
//      //this is only valid if WildcardSearchShouldNotFind works
//      "find a value by Fetch" in {
//        val lblSearch = "surveyChannel(level1(X), level2(Y))".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by Fetch" in {
//        val lblSearch = "surveyChannel(X, Y)".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
//
//      "find a channel by underscore Fetch" in {
//        val lblSearch = "surveyChannel(_, _)".toLabel
//        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      }
    }
  }

//  def testWildcardWithCursor(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) =
//  {
//    "Wildcard Search after store with Cursor" should {
//      Thread.sleep(timeoutBetween)
//
//      var resultQ = new LinkedBlockingQueue[ String ]
//      val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
//      val value = "tests@protegra.com"
//      storeWildcardData(writer, cnxnRandom, value)
//      val expectedCollection = setupExpectedResults(value)
//
//      "find many results by Get" in {
//        getCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "not find a value by Get" in {
//        getCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "not find a nested value by Get" in {
//        getCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "find many results by Fetch" in {
//        fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "not find a value by Fetch" in {
//        fetchCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "not find a nested value by Fetch" in {
//        fetchCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//
//      //
//      //       "find many results by Fetch" in {
//      //         fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//      //
//      //       "find many values by Fetch" in {
//      //         val lblSearch = "contentChannel(X)".toLabel
//      //         fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      //       }
//      //
//      //       "find many values by underscore Fetch" in {
//      //         val lblSearch = "contentChannel(_)".toLabel
//      //         fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//      //       }
//      //
//      //       "not find a value by Fetch" in {
//      //         val lblSearch = "fail(X)".toLabel
//      //         fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      //       }
//      //
//      //       "not find a nested value by Fetch" in {
//      //         val lblSearch = "abc(xyz(X))".toLabel
//      //         fetchMustBe("")(reader, cnxnRandom, lblSearch)
//      //       }
//      //
//      //       //this is only valid if WildcardSearchShouldNotFind works
//      //       "find a value by Fetch" in {
//      //         val lblSearch = "surveyChannel(level1(X))".toLabel
//      //         fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      //       }
//      //       //
//      //       //    "find a channel by Fetch" in {
//      //       //      val lblSearch = "surveyChannel(X)".toLabel
//      //       //      fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//      //       //    }
//      //
//      //     }
//
//    }
//  }
//
//  def testWildcardWithCursorBefore(writer: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], reader: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ]) =
//  {
//    "Wildcard Search before put with Cursor" should {
//      skip("BUG 54 - BROKEN until bug is fixed")
//      Thread.sleep(timeoutBetween)
//
//      val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
//      val value = "tests@protegra.com"
//
//      "find many results by Get" in {
//        val sync = new AnyRef()
//
//        val key = "contentChannel(_)"
//        var found = 0
//        val lblSearch = key.toLabel
//
//        reset {
//          for ( c <- reader.get(true)(cnxnRandom)(lblSearch) ) {
//            if ( c != None ) {
//              for ( e <- c.dispatchCursor ) {
//                println("*************getCount - received : " + e.dispatch.toString)
//                if ( e.dispatch.toString != "" ) {sync.synchronized {found += 1}}
//              }
//            }
//          }
//        }
//
//        putWildcardData(writer, cnxnRandom, value)
//
//        sync.synchronized {found} must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//
//      "find many results by Fetch" in {
//        val sync = new AnyRef()
//
//        val key = "contentChannel(_)"
//        var found = 0
//        val lblSearch = key.toLabel
//        reset {
//          for ( c <- reader.fetch(true)(cnxnRandom)(lblSearch) ) {
//            if ( c != None ) {
//              for ( e <- c.dispatchCursor ) {
//                println("*************getCount - received : " + e.dispatch.toString)
//                if ( e.dispatch.toString != "" ) {sync.synchronized {found += 1}}
//              }
//            }
//          }
//        }
//
//        putWildcardData(writer, cnxnRandom, value)
//
//        sync.synchronized {found} must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      }
//      //
//      //       "not find a value by Get" in {
//      //         getCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//      //
//      //       "not find a nested value by Get" in {
//      //         getCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//      //
//      //       "find many results by Fetch" in {
//      //         fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//      //
//      //       "not find a value by Fetch" in {
//      //         fetchCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//      //
//      //       "not find a nested value by Fetch" in {
//      //         fetchCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
//      //       }
//    }

}
