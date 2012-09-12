/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

package com.protegra_ati.agentservices.core.platformagents


import scala.collection.JavaConversions._
import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.OptionExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.util._
import org.junit._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import scala.util.Random
import java.net.URI
import org.specs.runner._
import com.protegra.agentservicesstore.AgentTS._
import com.biosimilarity.lift.lib.moniker._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import com.protegra.agentservicesstore.util.Severity
import com.protegra_ati.agentservices.core.util.serializer.Serializer


class BasePlatformAgentTest
  extends JUnit4(BasePlatformAgentTestSpecs)

object BasePlatformAgentTestSpecsRunner
  extends ConsoleRunner(BasePlatformAgentTestSpecs)

object BasePlatformAgentTestSpecs extends Specification
with RabbitTestSetup
with Timeouts
{
  val rand = new Random()

  val sourceId = UUID.randomUUID()
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

  def createPA: MockPlatformAgent =
  {
    val dbAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_DB)
    val privateAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
    val privateAcquaintanceAddresses = List[ URM ]()
    val pa = new MockPlatformAgent()
    // pa._cnxnUIStore = cnxn
    pa.initForTest(privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
    pa
  }

  //can't conflict on DB port
  def createPA1: MockPlatformAgent =
  {
    val dbAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PUBLIC_UNRELATED)
    val privateAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_UI_PRIVATE)
    val privateAcquaintanceAddresses = List[ URM ]("127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PRIVATE))
    val pa = new MockPlatformAgent()
    // pa._cnxnUIStore = cnxn
    pa.initForTest(privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
    pa
  }

  def createPA2: MockPlatformAgent =
  {
    val dbAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PUBLIC_UNRELATED)
    val privateAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
    val privateAcquaintanceAddresses = List[ URM ]("127.0.0.1".toURM.withPort(RABBIT_PORT_UI_PRIVATE))
    val pa = new MockPlatformAgent()
    // pa._cnxnUIStore = cnxn
    pa.initForTest(privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
    pa
  }

  //    @Ignore("these need to be real ips, use vms?") @Test
  //    def init() {
  //      val pa = new AgentHostStorePlatformAgent()
  //      var uri1 = "1.1.1.1".toURI
  //      var uri2 = "2.2.2.2".toURI
  //      var uri3 = "3.3.3.3".toURI
  //      pa.init(uri1, List(uri2, uri3), UUID.randomUUID)
  //      //didn't blow up
  //      assertTrue(true)
  //    }

  val pa = createPA

//  "send" should {
//    "not blow up" in {
//      try {
//        val queryObject = new Profile ()
//        val msg = new GetContentRequest(null, queryObject)
//
//        //fix this
//        //        pa.send(_privateQ, msg, sourceId, targetId)
//        true must be(true)
//      }
//      catch {
//        case _ => fail
//      }
//    }
//  }

//  "putGet" should {
//    val mockDataGet = new Profile("test", "me",  "test Description",  "123@test.com","CA", "someCAprovince", "city", "postalCode" )
//    val parentIds = new Identification()
//    val mockMsg = new GetContentResponse(parentIds.copyAsChild(), null, List(mockDataGet))
//    var result: Option[ GetContentResponse ] = None
//
//    "retrieve" in {
//      val key = "profile(\"putGet\")"
//      pa.put(pa._dbQ, cnxn, key, Serializer.serialize[ Message ](mockMsg))
//      //Thread.sleep(TIMEOUT_LONG)
//
//      pa.get(pa._dbQ, cnxn, key, handleGet)
//      result.value must be_==(mockMsg).eventually(20, TIMEOUT_EVENTUALLY)
//    }
//
//    def handleGet(cnxn: AgentCnxn, msg: Message) =
//    {
//      msg match {
//        case x: GetContentResponse => {
//          result = Some(x)
//        }
//        case _ => {
//        }
//      }
//    }
//  }
//
  "storeFetch Profile" should {
    val mockDataFetch = new Profile("test", "me",  "test Description", "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" )
    //remove mockDataSearch not needed
    val mockDataSearch = new Profile("test", "me",  "test Description", "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" )
    var result: Option[ Profile ] = None

    "retrieve" in {
      pa.store(pa._dbQ, cnxn, mockDataFetch.toStoreKey, Serializer.serialize[ Data ](mockDataFetch))
      fetchData(pa._dbQ, cnxn, mockDataFetch.toStoreKey) must be_==(mockDataFetch).eventually(20, TIMEOUT_EVENTUALLY)
    }
    "search" in {
         //use  mockDataFetch.toStoreKey
      pa.store(pa._dbQ, cnxn, mockDataSearch.toStoreKey, Serializer.serialize[ Data ](mockDataFetch))
      val search = new Profile ()
      search.id = mockDataSearch.id.toString
      fetchData(pa._dbQ, cnxn, search.toSearchKey) must be_==(mockDataFetch).eventually(20, TIMEOUT_EVENTUALLY)
    }

    def fetchData(queue: PartitionedStringMGJ, cnxn: AgentCnxn, searchKey: String): Any =
    {
      pa.fetch[ Data ](queue, cnxn, searchKey, handleFetch)
      return result.value
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      data match {
        case x: Profile => {
          result = Some(x)
        }
        case _ => {
        }
      }
    }
  }

  "storeListen" should {
    var result: Option[ GetContentRequest ] = None

    "listen and handle request" in {
      val mockProfile = new Profile("test", "me",  "test Description",  "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" )
      val mockSearch = new Profile ()
      mockSearch.id = mockProfile.id.toString
      val mockMsg = new GetContentRequest(new EventKey(UUID.randomUUID(), ""), mockSearch)

      pa.store(pa._privateQ, cnxn, mockMsg.getChannelKey, Serializer.serialize[ Message ](mockMsg))
      listen(pa._privateQ, cnxn, handlePrivateContentRequestChannel(_: AgentCnxn, _: Message)).value must be_==(mockMsg).eventually(20, TIMEOUT_EVENTUALLY)
    }

    def listen(q: PartitionedStringMGJ, cnxn: AgentCnxn, handler: (AgentCnxn, Message) => Unit) =
    {
      pa.listen(q, cnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, handler(_: AgentCnxn, _: Message))
      result
    }

    def handlePrivateContentRequestChannel[T <:Data](cnxn: AgentCnxn, msg: Message) =
    {
      msg match {
        case x: GetContentRequest => {
          result = Some(x)
        }
        case _ => {
        }
      }
    }
  }

  "delete" should {
    val mockProfile1 = new Profile("test1", "me",  "test Description",  "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" )
    var result: Option[ Profile ] = None

    "delete 1 record" in {
      pa.store(pa._dbQ, cnxn, mockProfile1.toStoreKey, Serializer.serialize[ Data ](mockProfile1))
      fetchData(pa._dbQ, cnxn, mockProfile1.toStoreKey) must be_==(Some(mockProfile1)).eventually(3, TIMEOUT_EVENTUALLY)
      pa.delete(pa._dbQ, cnxn, mockProfile1.toStoreKey)
      fetchData(pa._dbQ, cnxn, mockProfile1.toStoreKey) must be_==(None).eventually(3, TIMEOUT_EVENTUALLY)
    }

    def fetchData(queue: PartitionedStringMGJ, cnxn: AgentCnxn, key: String): Option[ Data ] =
    {
      result = None
      pa.fetch[ Data ](queue, cnxn, key, handleFetch)
      return result
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      data match {
        case x: Profile => {
          result = Some(x)
        }
        case _ => {
          result = None
        }
      }
    }
  }

  "createDrop" should {
    val newId = "Collection" + UUID.randomUUID
    val newCnxn = new AgentCnxn(newId.toURI, "", newId.toURI)

    val newProfile = new Profile("firstName", "lastName", "test Description", "111111111@test.com","CA", "someCAprovince", "city", "postalCode", "website" )
    var result: Option[ Profile ] = None

    "create and drop collection" in {
      pa.store(pa._dbQ, newCnxn, newProfile.toStoreKey, Serializer.serialize[ Data ](newProfile))
      //Thread.sleep(TIMEOUT_LONG)

      pa.drop(pa._dbQ, newCnxn)
      //Thread.sleep(TIMEOUT_LONG)

      fetchData(pa._dbQ, newCnxn, newProfile.toStoreKey) must be_==(None).eventually(20, TIMEOUT_EVENTUALLY)
    }

    def fetchData(queue: PartitionedStringMGJ, cnxn: AgentCnxn, key: String): Option[ Data ] =
    {
      result = None
      pa.fetch[ Data ](queue, cnxn, key, handleFetch)
      return result
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      data match {
        case x: Profile => result = Some(x)
        case _ => None
      }
    }
  }

  "listen from stored connection" should {
    val pa1 = createPA1
    val pa2 = createPA2
    var msgReceived = false
    val sync = new AnyRef ()

    //store connection
    val JenId = "Jen" + UUID.randomUUID
    val MikeId = "Mike" + UUID.randomUUID
    val conn = ConnectionFactory.createConnection("Jen", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", JenId.toString, MikeId.toString)
    val newCnxn = new AgentCnxn(UUID.randomUUID.toString.toURI, "", UUID.randomUUID.toString.toURI)
    pa1.store(pa1._dbQ, newCnxn, conn.toStoreKey, Serializer.serialize[ Data ](conn))
    Thread.sleep(TIMEOUT_LONG)

    //fetch connection
    val connSearch = new  Connection ()
    pa1.fetch[ Data ](pa1._dbQ, newCnxn, connSearch.toSearchKey, handleFetchConnection)
    //Thread.sleep(TIMEOUT_LONG)

    //listen on handler
    "receive message" in {
      val profileSearch = new Profile ()
      val request = GetContentRequest(EventKey(UUID.randomUUID(), ""), profileSearch)
      pa1.send(pa1._privateQ, conn.readCnxn, request)
      sync.synchronized {msgReceived} must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }

    def handleFetchConnection(cnxn: AgentCnxn, data: Data) =
    {
      data match {
        case x: Connection => {
          pa1.listen(pa1._privateQ, x.readCnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, listenHandler(_: AgentCnxn, _: Message))
        }
        case _ => {}
      }
    }

    def listenHandler(cnxn: AgentCnxn, msg: Message)
    {
      sync.synchronized {msgReceived = true}
    }
  }

}
