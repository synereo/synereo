/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

package com.protegra_ati.agentservices.core.platformagents


import scala.collection.JavaConversions._
import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.OptionExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.util._
import org.junit._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import scala.util.Random
import java.net.URI
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope._
import java.net.URI
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import com.protegra_ati.agentservices.store.util.Severity
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.protegra_ati.agentservices.core.util.Results
import org.specs2.specification.Scope

trait CnxnSetup extends Scope
with Serializable
{
  val sourceId = UUID.randomUUID()
  val targetId = sourceId
  val cnxn = new AgentCnxnProxy(sourceId.toString.toURI, "", targetId.toString.toURI)
}


class BasePlatformAgentTest extends SpecificationWithJUnit
with InitTestSetup
with Timeouts
with Serializable
{
  sequential

  @transient val rand = new Random()
  val pa = createPA

  def createPA: AgentHostStorePlatformAgent =
  {
//    val dbAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_DB)
//    val privateAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
//    val privateAcquaintanceAddresses = List[ URI ]()
    val pa = new AgentHostStorePlatformAgent()
    // pa._cnxnUIStore = cnxn
    pa.initFromConfig(CONFIG_STORE, UUID.randomUUID)
    pa
  }


  def handleData(cnxn: AgentCnxnProxy, data: Data, resultKey: String) =
  {
    data match {
      case x: Data => {
        Results.save(resultKey, x)
      }
      case _ => {
      }
    }
  }

  def handleMessage(cnxn: AgentCnxnProxy, msg: Message, resultKey: String) =
  {
    msg match {
      case x: Message => {
        Results.saveMessage(resultKey, x)
      }
      case _ => {
      }
    }
  }

  "putGet" should {

    "retrieve" in new CnxnSetup
    {
      val mockDataGet = new Profile("test", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website", null)
      val parentIds = new Identification()
      val mockMsg = new GetContentResponse(parentIds.copyAsChild(), null, List(mockDataGet))

      val key = "profile(\"putGet\")"
      pa.put(pa._dbQ, cnxn, key, Serializer.serialize[ Message ](mockMsg))

      val resultKey = Results.getKey
      pa.get(pa._dbQ, cnxn, key, handleMessage(_: AgentCnxnProxy, _: Message, resultKey))
      Results.savedMessage(resultKey) must be_==(mockMsg).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

  "storeFetch Profile" should {
    "retrieve" in new CnxnSetup
    {
      val mockDataFetch = new Profile("test", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

      pa.store(pa._dbQ, cnxn, mockDataFetch.toStoreKey, Serializer.serialize[ Data ](mockDataFetch))

      val resultKey = Results.getKey
      Thread.sleep(TIMEOUT_MED)
      pa.fetch[ Data ](pa._dbQ, cnxn, mockDataFetch.toSearchKey, handleData(_: AgentCnxnProxy, _: Data, resultKey))
      Results.saved(resultKey) must be_==(mockDataFetch).eventually(5, TIMEOUT_EVENTUALLY)
    }
    "search" in new CnxnSetup
    {
      val mockDataFetch = new Profile("test", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      val mockDataSearch = new Profile("test", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

      pa.store(pa._dbQ, cnxn, mockDataSearch.toStoreKey, Serializer.serialize[ Data ](mockDataFetch))

      val search = new Profile()
      search.id = mockDataSearch.id.toString
      val resultKey = Results.getKey
      Thread.sleep(TIMEOUT_MED)
      pa.fetch[ Data ](pa._dbQ, cnxn, search.toSearchKey, handleData(_: AgentCnxnProxy, _: Data, resultKey))
      Results.saved(resultKey) must be_==(mockDataFetch).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }
  "listen and send on rabbit" should {
    "handle request" in new CnxnSetup
    {
      val mockProfile = new Profile("test", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      val mockSearch = new Profile()
      mockSearch.id = mockProfile.id.toString
      val mockMsg = new GetContentRequest(new EventKey(UUID.randomUUID(), ""), mockSearch)

      val resultKey = Results.getKey
      pa.listenRabbit(pa._privateRabbitConfig, cnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, handleMessage(cnxn, _: Message, resultKey))
      pa.sendRabbit(pa._privateRabbitConfig, cnxn, mockMsg)
      Results.savedMessage(resultKey) must be_==(mockMsg).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

  def fetchData(queue: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxnProxy, key: String): Data =
  {
    val resultKey = Results.getKey
    pa.fetch[ Data ](queue, cnxn, key, handleData(_: AgentCnxnProxy, _: Data, resultKey))
    Results.saved(resultKey)
  }

  "delete" should {

    "delete 1 record" in new CnxnSetup {
      val mockProfile1 = new Profile("test1", "me", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

      pa.store(pa._dbQ, cnxn, mockProfile1.toStoreKey, Serializer.serialize[ Data ](mockProfile1))

      fetchData(pa._dbQ, cnxn, mockProfile1.toStoreKey) must be_==(mockProfile1).eventually(5, TIMEOUT_EVENTUALLY)
      Thread.sleep(TIMEOUT_LONG)
      pa.delete(pa._dbQ, cnxn, mockProfile1.toStoreKey)
      fetchData(pa._dbQ, cnxn, mockProfile1.toStoreKey) must beNull.eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

    "createDrop" should {

      "create and drop collection" in {
        val newId = "Collection" + UUID.randomUUID
        val newCnxn = new AgentCnxnProxy(newId.toURI, "", newId.toURI)

        val newProfile = new Profile("firstName", "lastName", "test Description", "111111111@test.com","CA", "someCAprovince", "city", "postalCode", "website" )

        pa.store(pa._dbQ, newCnxn, newProfile.toStoreKey, Serializer.serialize[ Data ](newProfile))
        Thread.sleep(TIMEOUT_LONG)

        pa.drop(pa._dbQ, newCnxn)
        Thread.sleep(TIMEOUT_LONG)

        fetchData(pa._dbQ, newCnxn, newProfile.toStoreKey) must beNull.eventually(5, TIMEOUT_EVENTUALLY)
      }

    }

//      //can't conflict on DB port
//  def createPA1: MockPlatformAgent =
//  {
//    val dbAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PUBLIC_UNRELATED)
//    val privateAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
//    val privateAcquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE))
//    val pa = new MockPlatformAgent()
//    // pa._cnxnUIStore = cnxn
//    pa.initFromConfig(privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
//    pa
//  }
//
//  def createPA2: MockPlatformAgent =
//  {
//    val dbAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PUBLIC_UNRELATED)
//    val privateAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
//    val privateAcquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_UI_PRIVATE))
//    val pa = new MockPlatformAgent()
//    // pa._cnxnUIStore = cnxn
//    pa.initFromConfig(privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
//    pa
//  }

  //  "listen from stored connection" should {
  //    @transient val pa1 = createPA1
  ////    @transient val pa2 = createPA2
  //    var msgReceived = false
  //
  //    //store connection
  //    val jenId = "Jen" + UUID.randomUUID
  //    val mikeId = "Mike" + UUID.randomUUID
  //    val conn = ConnectionFactory.createConnection("Jen", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Trusted", jenId.toString, mikeId.toString)
  //    val newCnxn = new AgentCnxnProxy(UUID.randomUUID.toString.toURI, "", UUID.randomUUID.toString.toURI)
  //    pa1.store(pa1._dbQ, newCnxn, conn.toStoreKey, Serializer.serialize[ Data ](conn))
  //    Thread.sleep(TIMEOUT_LONG)
  //
  //    //fetch connection
  //    val connSearch = new  Connection ()
  //    pa1.fetch[ Data ](pa1._dbQ, newCnxn, connSearch.toSearchKey, handleFetchConnection)
  //    //Thread.sleep(TIMEOUT_LONG)
  //
  //    //listen on handler
  //    "receive message" in {
  //      val profileSearch = new Profile ()
  //      val request = GetContentRequest(EventKey(UUID.randomUUID(), ""), profileSearch)
  //      pa1.send(pa1._privateQ, conn.readCnxn, request)
  //      msgReceived must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
  //    }
  //
  //    def handleFetchConnection(cnxn: AgentCnxnProxy, data: Data) =
  //    {
  //      data match {
  //        case x: Connection => {
  //          pa1.listen(pa1._privateQ, x.readCnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, listenHandler(_: AgentCnxnProxy, _: Message))
  //        }
  //        case _ => {}
  //      }
  //    }
  //
  //    def listenHandler(cnxn: AgentCnxnProxy, msg: Message)
  //    {
  //      msgReceived = true
  //    }
  //  }

}


