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

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.AgentTS.mTT._

import actors.threadpool.LinkedBlockingQueue
import com.protegra.agentservicesstore.extensions.LinkedBlockingQueueExtensions._

import scala.concurrent.ops._
import com.biosimilarity.lift.lib.moniker._

abstract class KvdbPlatformAgentBase extends Specification
with SpecsKVDBHelpers
with Timeouts
{
  val timeoutBetween: Int

  def createJunction(sourceAddress: URM, acquaintanceAddresses: List[URM]): PartitionedStringMGJ = new PartitionedStringMGJ(sourceAddress, acquaintanceAddresses, None)

  //we use this so we can test same/same, local1/local2, remote1/remote2
  //this prevents copy paste and so makes sure the same set of tests is run on each
  def testMessaging(writer: PartitionedStringMGJ, reader: PartitionedStringMGJ) =
  {
    val sourceId = UUID.randomUUID
    val targetId = sourceId
    val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)
    val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)

    "Cached Put/Get" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(cachedPutGetRespectingCnxn(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        getMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(cachedPutGet(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}

        //first get takes it out of cache
        getMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ("testChannel(\"" + UUID.randomUUID.toString + "\")").toLabel
        val value = "uuid@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}

        getMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(cachedPutGet(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value + "1"))}
        reset {writer.put(cnxn)(key, Ground(value + "2"))}
        reset {writer.put(cnxn)(key, Ground(value + "3"))}

        getMustBe(value + "3")(writer, cnxn, key)
      }
    }

    //invalid when #23 resolved
    "Persisted Put/Get" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(persistedPutGetRespectingCnxn(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}

        //first get takes it out of cache
        getMustBe("")(reader, cnxnRandom, key)
        //second get takes it out db
        //Thread.sleep(TIMEOUT_MED)
        getMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(persistedPutGet(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}

        Thread.sleep(timeoutBetween)
        //Thread.sleep(TIMEOUT_LONG)
        //first get takes it out of cache
        getMustBe(value)(reader, cnxn, key)
        //second get takes it out db
        getMustBe(value)(reader, cnxn, key)
      }

//      "work with UUID" in {
//        val key = ("testChannel(\"" + UUID.randomUUID.toString + "\")").toLabel
//        val value = "uuid@protegra.com"
//        reset {writer.put(cnxn)(key, Ground(value))}
//
//        //first get takes it out of cache
//        getMustBe(value)(reader, cnxn, key)
//        //second get takes it out db
//        //Thread.sleep(TIMEOUT_MED)
//        getMustBe(value)(reader, cnxn, key)
//      }

      "retrieve from existing db" in {
        val cnxnNew = new AgentCnxn("Test".toURI, "", "NewDB".toURI)

        val key = "testChannel(newData(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxnNew)(key, Ground(value))}

        //first get takes it out of cache
        getMustBe(value)(reader, cnxnNew, key)
        //second get takes it out db
        //Thread.sleep(TIMEOUT_MED)
        getMustBe(value)(reader, cnxnNew, key)
      }

      "update" in {
        val key = "contentChannel(persistedPutGet(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value + "1"))}
        Thread.sleep(TIMEOUT_MED)
        reset {writer.put(cnxn)(key, Ground(value + "2"))}
        Thread.sleep(TIMEOUT_MED)
        reset {writer.put(cnxn)(key, Ground(value + "3"))}
        Thread.sleep(TIMEOUT_MED)

        //first get takes it out of cache
        getMustBe(value + "3")(writer, cnxn, key)
        //second get takes it out db
        Thread.sleep(TIMEOUT_SHORT)
        getMustBe(value + "3")(writer, cnxn, key)
      }
    }

    "Cached Put/Fetch" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(cachedPutFetchRespectingCnxn(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        fetchMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(cachedPutFetch(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        fetchMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ("testChannel(\"" + UUID.randomUUID.toString + "\")").toLabel
        val value = "uuid@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value))}
        fetchMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(cachedPutFetch(\"email\"))".toLabel
        val value = "tests@protegra.com"
        reset {writer.put(cnxn)(key, Ground(value + "1"))}
        reset {writer.put(cnxn)(key, Ground(value + "2"))}
        reset {writer.put(cnxn)(key, Ground(value + "3"))}

        fetchMustBe(value + "3")(writer, cnxn, key)
      }
    }

    "Store/Get" should {
      Thread.sleep(timeoutBetween)

      "respect connection" in {
        val key = "testChannel(storeGetRespectingCnxn(\"email\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        getMustBe("")(reader, cnxnRandom, key)
      }

      "retrieve " in {
        val key = "contentChannel(storeFetch(\"email\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_LONG)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "work with UUID" in {
        val key = ("testChannel(\"" + UUID.randomUUID.toString + "\")").toLabel
        val value = "uuid@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_LONG)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "retrieve from existing db" in {
        val cnxnNew = new AgentCnxn("Test".toURI, "", "NewDB".toURI)

        val key = "testChannel(testData(\"name\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxnNew)(key, Ground(value))

        //Thread.sleep(TIMEOUT_LONG)
        getMustBe(value)(reader, cnxnNew, key)
      }

      "update" in {
        val key = "contentChannel(updateStoreGet(\"email\"))".toLabel
        val value = "tests@protegra.com"

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
        val value = "tests@protegra.com"
        writer.store(cnxnRandom)(key, Ground(value))

        //Thread.sleep(TIMEOUT_MED)
        //first get takes it out of db
        getMustBe(value)(reader, cnxnRandom, key)

        //Thread.sleep(TIMEOUT_MED)
        //second get should not find
        getMustBe("")(reader, cnxnRandom, key)
      }
    }

    "Store/Fetch" should {
      Thread.sleep(timeoutBetween)
      "respect connection" in {
        val key = "testChannel(storeFetchRespectingCnxn(\"email\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        fetchMustBe("")(reader, cnxnRandom, key)
      }

//      "retrieve " in {
//        val key = "contentChannel(storeFetch(\"email\"))".toLabel
//        val value = "tests@protegra.com"
//        writer.store(cnxn)(key, Ground(value))
//        //Thread.sleep(TIMEOUT_MED)
//        fetchMustBe(value)(reader, cnxn, key)
//      }

      "work with UUID" in {
        val key = ("testChannel(\"" + UUID.randomUUID.toString + "\")").toLabel
        val value = "uuid@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
      }

      "update" in {
        val key = "contentChannel(updateStoreFetch(\"email\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxn)(key, Ground(value + "1"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "2"))
        Thread.sleep(TIMEOUT_MED)
        writer.store(cnxn)(key, Ground(value + "3"))

        //wont break but theres actually 3 results in the db
        fetchMustBe(value + "3")(reader, cnxn, key)
      }

      "search" in {
        val key = "profile(\"999\",\"Terry\",\"Bunio\",\"123456789\")".toLabel
        val value = "profile for terry bunio"
        writer.store(cnxn)(key, Ground(value))
        //Thread.sleep(TIMEOUT_MED)
        val searchKey = "profile(\"999\",_,_,_)".toLabel
        fetchMustBe(value)(reader, cnxn, searchKey)
      }
    }

    "Cached Get/Put" should {
      Thread.sleep(timeoutBetween)
      "retrieve" in {
        val key = "contentChannel(cacheGetPut(\"email\"))".toLabel
        val value = "tests@protegra.com"

        var result = ""
        reset {
          for (e <- reader.get(cnxn)(key)) {
            result = e.dispatch
            println("get received : " + result)
          }
        }
        reset {writer.put(cnxn)(key, Ground(value))}
        result must be_==(value).eventually(20, TIMEOUT_EVENTUALLY)
      }
    }

    "Cached Fetch/Put" should {
      Thread.sleep(timeoutBetween)
      "retrieve" in {
        val key = "contentChannel(cachedFetchPut(\"email\"))".toLabel
        val value = "tests@protegra.com"

        var result = ""
        reset {
          for (e <- reader.fetch(cnxn)(key)) {
            result = e.dispatch
            println("Fetch received : " + result)
          }
        }
        reset {writer.put(cnxn)(key, Ground(value))}
        result must be_==(value).eventually(20, TIMEOUT_EVENTUALLY)
      }
    }

    "Get/Store" should {
    // get/fetch before store does put a waiter
    }

    "Fetch/Store" should {
    // get/fetch before store does put a waiter
    }

    "Multiple Get/Put" should {
      Thread.sleep(timeoutBetween)
      "retrieve" in {
        val key = "contentChannel(multipleGetPut(\"email\"))".toLabel
        val value = "tests@protegra.com"
        var result = ""
        reset {
          for (e <- reader.get(cnxn)(key)) {
            result = e.dispatch
            println("get received : " + result)
          }
        }
        reset {writer.put(cnxn)(key, Ground(value))}
        result must be_==(value).eventually(20, TIMEOUT_EVENTUALLY)
        //is this necessary?
        //Thread.sleep(TIMEOUT_MED)
        getMustBe("")(reader, cnxn, key)
        getMustBe("")(reader, cnxn, key)
      }
    }

    "Multiple Fetch/Put" should {
      Thread.sleep(timeoutBetween)
      "retrieve" in {
        val key = "contentChannel(multipleFetchPut(\"email\"))".toLabel
        val value = "tests@protegra.com"
        var result = ""
        reset {
          for (e <- reader.fetch(cnxn)(key)) {
            result = e.dispatch
            println("get received : " + result)
          }
        }
        reset {writer.put(cnxn)(key, Ground(value))}
        result must be_==(value).eventually(20, TIMEOUT_EVENTUALLY)
        //is this necessary?
        //Thread.sleep(TIMEOUT_MED)
        fetchMustBe("")(reader, cnxn, key)
        fetchMustBe("")(reader, cnxn, key)

      }
    }

    "Multiple Fetch/Store" should {
    // get/fetch before store does put a waiter
    }

    "Multiple Store/Fetch" should {
      Thread.sleep(timeoutBetween)
      "retrieve" in {
        val key = "contentChannel(multipleStoreFetch(\"email\"))".toLabel
        val value = "tests@protegra.com"
        writer.store(cnxn)(key, Ground(value))
        //is this really necessary?
        //Thread.sleep(TIMEOUT_MED)
        fetchMustBe(value)(reader, cnxn, key)
        fetchMustBe(value)(reader, cnxn, key)
        fetchMustBe(value)(reader, cnxn, key)
      }
    }
  }

  def testWildcardWithPut(writer: PartitionedStringMGJ, reader: PartitionedStringMGJ) = testWildcard(writer, reader, "put", putWildcardData(_, _, _))

  def testWildcardWithStore(writer: PartitionedStringMGJ, reader: PartitionedStringMGJ) = testWildcard(writer, reader, "store", storeWildcardData(_, _, _))

  private def putWildcardData(writer: PartitionedStringMGJ, cnxn: AgentCnxn, value: String) =
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
  private def storeWildcardData(writer: PartitionedStringMGJ, cnxn: AgentCnxn, value: String) =
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

  private def setupExpectedResults(value:String) = {
    val expectedResults = List(1,2,3,4,5) map (f => value + f.toString)
    val expectedCollection:java.util.List[String] = new java.util.ArrayList[String]
    for (p <- expectedResults) expectedCollection.add(p)
    expectedCollection
  }

  private def testWildcard(writer: PartitionedStringMGJ, reader: PartitionedStringMGJ, storageMethod: String, setupData: (PartitionedStringMGJ, AgentCnxn, String) => Unit) =
  {
    "Wildcard Search after " + storageMethod should {
      Thread.sleep(timeoutBetween)

      var resultQ = new LinkedBlockingQueue[String]
      val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
      val value = "tests@protegra.com"
      setupData(writer, cnxnRandom, value)
      val expectedCollection = setupExpectedResults(value)

      "find many results by Get" in {
        println("attempting assert")
        getCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
      }

      "find many values by Get" in {
        val lblSearch = "contentChannel(X)".toLabel
        getMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
      }

      "find many values by underscore Get" in {
        val lblSearch = "contentChannel(_)".toLabel
        getMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
      }

      "not find a value by Get" in {
        val lblSearch = "fail(X)".toLabel
        getMustBe("")(reader, cnxnRandom, lblSearch)
      }

      "not find a nested value by Get" in {
        val lblSearch = "abc(xyz(X))".toLabel
        getMustBe("")(reader, cnxnRandom, lblSearch)
      }

      //this is only valid if WildcardSearchShouldNotFind works
      "find a value by Get" in {
//        skip("specs error")
        val lblSearch = "surveyChannel(level1(X), level2(X))".toLabel
        getMustBe(value)(reader, cnxnRandom, lblSearch)
      }

      "find a channel by Get" in {
        val lblSearch = "surveyChannel(X, X)".toLabel
        getMustBe(value)(reader, cnxnRandom, lblSearch)
      }

      "find a channel by underscore Get" in {
        val lblSearch = "surveyChannel(_, _)".toLabel
        getMustBe(value)(reader, cnxnRandom, lblSearch)
      }


//      "find many results by Fetch" in {
//        Thread.sleep(TIMEOUT_LONG)
//        fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
////        countMustBe(5)(reader, cnxnRandom, "contentChannel(X)")
//      }

      "find many values by Fetch" in {
        val lblSearch = "contentChannel(X)".toLabel
        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
      }

      "find many values by underscore Fetch" in {
//        skip("specs error")
        val lblSearch = "contentChannel(_)".toLabel
        fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
      }

      "not find a value by Fetch" in {
        val lblSearch = "fail(X)".toLabel
        fetchMustBe("")(reader, cnxnRandom, lblSearch)
      }

      "not find a nested value by Fetch" in {
        val lblSearch = "abc(xyz(X))".toLabel
        fetchMustBe("")(reader, cnxnRandom, lblSearch)
      }

      //this is only valid if WildcardSearchShouldNotFind works
      "find a value by Fetch" in {
        val lblSearch = "surveyChannel(level1(X), level2(X))".toLabel
        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
      }

      "find a channel by Fetch" in {
        val lblSearch = "surveyChannel(X, X)".toLabel
        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
      }

      "find a channel by underscore Fetch" in {
        val lblSearch = "surveyChannel(_, _)".toLabel
        fetchMustBe(value)(reader, cnxnRandom, lblSearch)
      }

      "find many results by Fetch" in {
        countMustBe(5)(reader, cnxnRandom, "contentChannel(X)")
//        fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
       }


    }
  }

  def testWildcardWithCursor(writer: PartitionedStringMGJ, reader: PartitionedStringMGJ) = {
    "Wildcard Search after store with Cursor" should {
       Thread.sleep(timeoutBetween)

       var resultQ = new LinkedBlockingQueue[String]
       val cnxnRandom = new AgentCnxn("Random".toURI, "", UUID.randomUUID.toString.toURI)
       val value = "tests@protegra.com"
       storeWildcardData(writer, cnxnRandom, value)
       val expectedCollection = setupExpectedResults(value)

       "find many results by Get" in {
         getCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
       }

       "not find a value by Get" in {
         getCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(20, TIMEOUT_EVENTUALLY)
       }

       "not find a nested value by Get" in {
         getCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(20, TIMEOUT_EVENTUALLY)
       }

       "find many results by Fetch" in {
         fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
       }

       "not find a value by Fetch" in {
         fetchCount(reader, cnxnRandom, "fail(X)") must be_==(0).eventually(20, TIMEOUT_EVENTUALLY)
       }

       "not find a nested value by Fetch" in {
         fetchCount(reader, cnxnRandom, "abc(xyz(X))") must be_==(0).eventually(20, TIMEOUT_EVENTUALLY)
       }


//
//       "find many results by Fetch" in {
//         fetchCount(reader, cnxnRandom, "contentChannel(X)") must be_==(5).eventually(20, TIMEOUT_EVENTUALLY)
//       }
//
//       "find many values by Fetch" in {
//         val lblSearch = "contentChannel(X)".toLabel
//         fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//       }
//
//       "find many values by underscore Fetch" in {
//         val lblSearch = "contentChannel(_)".toLabel
//         fetchMustContain(expectedCollection, resultQ)(reader, cnxnRandom, lblSearch)
//       }
//
//       "not find a value by Fetch" in {
//         val lblSearch = "fail(X)".toLabel
//         fetchMustBe("")(reader, cnxnRandom, lblSearch)
//       }
//
//       "not find a nested value by Fetch" in {
//         val lblSearch = "abc(xyz(X))".toLabel
//         fetchMustBe("")(reader, cnxnRandom, lblSearch)
//       }
//
//       //this is only valid if WildcardSearchShouldNotFind works
//       "find a value by Fetch" in {
//         val lblSearch = "surveyChannel(level1(X))".toLabel
//         fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//       }
//       //
//       //    "find a channel by Fetch" in {
//       //      val lblSearch = "surveyChannel(X)".toLabel
//       //      fetchMustBe(value)(reader, cnxnRandom, lblSearch)
//       //    }
//
//     }

    }
  }

}