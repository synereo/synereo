package com.protegra_ati.agentservices.core.platformagents

import behaviors.Storage
import org.specs._
import com.protegra_ati.agentservices.core._

import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import org.junit._
import Assert._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import java.net.URI
import com.protegra_ati.agentservices.core.messages.content._
//import com.protegra_ati.agentservices.core.messages.search._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.disclosure._
import com.protegra_ati.agentservices.core.schema.PersistedRequest
import invitation._
import scala.util.Random

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.events._
import com.biosimilarity.lift.lib.moniker._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import java.util.{Locale, UUID}
//import search.SearchResponse
//import com.protegra.config.ConfigurationManager
import com.protegra_ati.agentservices.core.util.serializer.Serializer


object AgentHostCombinedBase extends Specification with RabbitTestSetup with
Timeouts
{

  // inits test configuration
//  ConfigurationManager.getConfigurationManager().initForTest()

  val cnxnUIStore = new AgentCnxn(( "UI" + UUID.randomUUID().toString ).toURI, "", ( "Store" + UUID.randomUUID().toString ).toURI);
  val uiRef: AgentHostUIPlatformAgent = new AgentHostUIPlatformAgent()
  val storeRef: AgentHostStorePlatformAgent = new AgentHostStorePlatformAgent()


  val setup = new SpecContext
  {
    println(" in SharedContext constructor:")
    // ui
    val privateAddressUI = "localhost".toURM.withPort(RABBIT_PORT_UI_PRIVATE)
    val privateAcquaintanceAddressesUI = List[ URM ]("localhost".toURM.withPort(RABBIT_PORT_STORE_PRIVATE))
    uiRef._cnxnUIStore = cnxnUIStore
    val idUI = UUID.randomUUID
    uiRef.initForTest(privateAddressUI, privateAcquaintanceAddressesUI, idUI)

    // store
    val publicAddress = "localhost".toURM.withPort(RABBIT_PORT_STORE_PUBLIC)
    val publicAcquaintanceAddresses = List[ URM ]("localhost".toURM.withPort(RABBIT_PORT_STORE_PUBLIC_UNRELATED))
    val privateAddress = "localhost".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
    val privateAcquaintanceAddresses = List[ URM ]("localhost".toURM.withPort(RABBIT_PORT_UI_PRIVATE))
    val dbAddress = "localhost".toURM.withPort(RABBIT_PORT_STORE_DB)
    val resultAddress = "localhost".toURM.withPort(RABBIT_PORT_TEST_RESULTS_DB)
    storeRef._cnxnUIStore = cnxnUIStore
    val id = UUID.randomUUID
    //storeRef._cnxnUserSelfConnectionsList = List(cnxnJenSelf, cnxnMikeSelf)
    storeRef.initForTest(publicAddress, publicAcquaintanceAddresses, privateAddress, privateAcquaintanceAddresses, dbAddress, resultAddress, id)
    //?
    //base.setupIncrementalDisclosure(storeRef, cnxnJenSelf)
  }
  setup.beforeSpec {
    // demo
    println(" --- beforeSpec, SharedContext solution")
  }


  def setupIncrementalDisclosure(pa: AgentHostStorePlatformAgent, cnxn: AgentCnxn) =
  {
//    ProfileDisclosedDataFactory
    // new DisclosedData[ Profile ](classOf[ Profile ], "Empty", "")
    val authorizedContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
    //new DisclosedData[ Profile ](classOf[ Profile ], "Basic", "id,localeCode,firstName,lastName")
    val authorizedContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
    // new DisclosedData[ Profile ](classOf[ Profile ], "Full", "id,localeCode,firstName,lastName,description,emailAddress,country,region,city")
    val authorizedContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)
    //save Profile authorized content for basic and full
    pa.store(pa._dbQ, cnxn, authorizedContentEmpty.toStoreKey, Serializer.serialize[ Data ](authorizedContentEmpty))
    pa.store(pa._dbQ, cnxn, authorizedContentBasic.toStoreKey, Serializer.serialize[ Data ](authorizedContentBasic))
    pa.store(pa._dbQ, cnxn, authorizedContentFull.toStoreKey, Serializer.serialize[ Data ](authorizedContentFull))
    Thread.sleep(TIMEOUT_LONG)

    val appIdFull = AppIdDisclosedDataFactory.getDisclosedData(TrustLevel.Full) //DisclosedData[ AppId ](classOf[ AppId ], "Full", "id,localeCode,name");
    val appIdBasic = AppIdDisclosedDataFactory.getDisclosedData(TrustLevel.Basic) //DisclosedData[ AppId ](classOf[ AppId ], "Basic", "id,localeCode,name");
    val appIdIntroduced = AppIdDisclosedDataFactory.getDisclosedData(TrustLevel.Introduced) //DisclosedData[ AppId ](classOf[ AppId ], "Introduced", "id,localeCode,name");

    //save appId authorized content for basic basic full
    pa.store(pa._dbQ, cnxn, appIdFull.toStoreKey, Serializer.serialize[ Data ](appIdFull))
    pa.store(pa._dbQ, cnxn, appIdBasic.toStoreKey, Serializer.serialize[ Data ](appIdBasic))
    pa.store(pa._dbQ, cnxn, appIdIntroduced.toStoreKey, Serializer.serialize[ Data ](appIdIntroduced))
    Thread.sleep(TIMEOUT_LONG)
  }

  def getConnection(selfId: String, targetId: String): Connection =
  {
    val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.None.toString, ConnectionCategory.None.toString, "Introduced", selfId, targetId);
    newConn
  }

  def setupConnection(store: AgentHostStorePlatformAgent, selfId: UUID, targetId: UUID): Connection =
  {
    setupConnection(store, selfId.toString, targetId.toString)
  }

  def setupConnection(store: AgentHostStorePlatformAgent, selfId: String, targetId: String): Connection =
  {
    val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.None.toString, ConnectionCategory.None.toString, "Introduced", selfId, targetId);
    val sourceSelfCnxn = new AgentCnxn(newConn.writeCnxn.src, "", newConn.writeCnxn.src)

    store.processNewConnection(newConn, sourceSelfCnxn)
    Thread.sleep(TIMEOUT_SHORT)
    newConn
  }

  def setupConnection(store: AgentHostStorePlatformAgent, newConn: Connection): Connection =
  {
    val sourceSelfCnxn = new AgentCnxn(newConn.writeCnxn.src, "", newConn.writeCnxn.src)
    store.processNewConnection(newConn, sourceSelfCnxn)
    Thread.sleep(TIMEOUT_SHORT)
    newConn
  }

  def setupPersistedConnection(pa: AgentHostStorePlatformAgent, selfId: UUID, targetId: UUID): Connection =
  {
    setupPersistedConnection(pa, selfId.toString, targetId.toString)
  }

  def setupPersistedConnection(pa: AgentHostStorePlatformAgent, selfId: String, targetId: String): Connection =
  {
    val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.None.toString, ConnectionCategory.None.toString, "Full", selfId, targetId);
    val sourceSelfCnxn = new AgentCnxn(newConn.writeCnxn.src, "", newConn.writeCnxn.src)

    pa.processNewConnection(newConn, sourceSelfCnxn)
    if ( selfId != targetId )
      pa.updateData(sourceSelfCnxn, newConn, null)

    Thread.sleep(TIMEOUT_SHORT)
    newConn
  }

  /**
   * Persists given connection
   * @param pa store
   * @param selfTargetConn conn. to be persisted
   * @return reference to  selfTargetConn passed truth
   */
  def setupPersistedConnection(pa: AgentHostStorePlatformAgent, selfTargetConn: Connection): Connection =
  {
    val sourceSelfCnxn = new AgentCnxn(selfTargetConn.writeCnxn.src, "", selfTargetConn.writeCnxn.src)

    pa.processNewConnection(selfTargetConn, sourceSelfCnxn)
    if ( !selfTargetConn.readCnxn.src.toString.equals(selfTargetConn.readCnxn.trgt.toString) )
      pa.updateData(sourceSelfCnxn, selfTargetConn, null)

    Thread.sleep(TIMEOUT_SHORT)
    selfTargetConn
  }


  def setupPersistedConnection(pa: AgentHostStorePlatformAgent, alias: String, connectionType: String, selfId: String, targetId: String): Connection =
  {
    val newConn = ConnectionFactory.createConnection(alias, ConnectionCategory.None.toString, ConnectionCategory.None.toString, connectionType, selfId, targetId);
    val sourceSelfCnxn = new AgentCnxn(newConn.writeCnxn.src, "", newConn.writeCnxn.src)

    pa.processNewConnection(newConn, sourceSelfCnxn)
    if ( selfId != targetId )
      pa.updateData(sourceSelfCnxn, newConn, null)

    Thread.sleep(TIMEOUT_SHORT)
    newConn
  }

  def setupSelfConnection(pa: AgentHostStorePlatformAgent, cnxn: AgentCnxn, alias: String, id: String, connectionType: String): Connection =
  {
    val self = ConnectionFactory.createConnection(alias, ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, connectionType, id, id)
    pa.store(pa._dbQ, cnxn, self.toStoreKey, Serializer.serialize[ Data ](self))
    storeRef.generateSystemData(self.writeCnxn, self)
    //
    self
  }

  def setAppId(ui: AgentHostUIPlatformAgent, conn: Connection, businessName: String) =
  {
    val msg = new SetContentRequest(new EventKey(UUID.randomUUID, ""), AppId(businessName), null)
    msg.targetCnxn = conn.writeCnxn
    msg.originCnxn = ui._cnxnUIStore
    ui.send(msg)
    Thread.sleep(500)
  }

  def setProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, localeCode: String): Unit =
  {
    val Profile = new Profile("testFirst", "testLast", "testDesc", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
    setProfile(ui, cnxn, agentSessionId, tag, localeCode, Profile)
  }

  def setProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String): Unit =
  {
    val Profile = new Profile("testFirst", "testLast", "testDesc", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
    setProfile(ui, cnxn, agentSessionId, tag, Locale.ENGLISH.toString(), Profile)
  }

  def setProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, localeCode: String, Profile: Profile): Unit =
  {
    Profile.localeCode = localeCode
    val setReq = new SetContentRequest(new EventKey(agentSessionId, tag), Profile, null)
    setReq.originCnxn = cnxn
    setReq.targetCnxn = cnxn
    ui.send(setReq)
  }

  def setAppId(ui: AgentHostUIPlatformAgent, self: Connection): AppId =
  {
    val appId = AppId("TestApp")
    appId.setDefaultValues(false)
    setContent(ui, self, appId)
    appId
  }

  def setContent(ui: AgentHostUIPlatformAgent, conn: Connection, data: Data) =
  {
    val msg = new SetContentRequest(new EventKey(UUID.randomUUID, ""), data, null)

    msg.targetCnxn = conn.writeCnxn
    msg.originCnxn = conn.readCnxn
    ui.send(msg)
    Thread.sleep(500)

  }


  def countProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String): Int =
  {
    countProfile(ui, cnxn, agentSessionId, tag, None)

  }

  def countProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, localeCode: Option[ String ]): Int =
  {
    val query: Profile = new Profile()
    localeCode match {
      case Some(x) => query.localeCode = x
      case _ => {}
    }
    count(ui, cnxn, agentSessionId, tag, query)
  }

  def countPersistedInvitationRequests(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, localeCode: Option[ String ]): Int =
  {
    val query: PersistedMessage[ InvitationRequest ] = new PersistedMessage[ InvitationRequest ]()
    localeCode match {
      case Some(x) => query.localeCode = x
      case _ => {}
    }
    count(ui, cnxn, agentSessionId, tag, query)
  }

  def count(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: Data): Int =
  {
    val sync = new AnyRef()
    //tag needs to be random otherwise only the 1st listen will wake up by the time the 4th listen is applying the must be_==
    //we intend to do many separate listens
    @volatile var count = 0
    val tagUnique = tag + UUID.randomUUID().toString
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tagUnique)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        println("===========================getContentResponseReceived: " + e)
        e.msg match {
          case x: GetContentResponse => {
            sync.synchronized {count = x.data.size}
          }
          case _ => {}
        }
      }
    });

    val getReq = new GetContentRequest(new EventKey(agentSessionId, tagUnique), query)
    getReq.originCnxn = cnxn
    getReq.targetCnxn = cnxn
    ui.send(getReq)

    trySleep(sync.synchronized {count})
    sync.synchronized {count}
  }

  def countCompositeProfile(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String): Int =
  {
    val query = new CompositeData[ Profile ](new Connection(), new Profile());
    countComposite(ui, cnxn, agentSessionId, tag, query)
  }

  def countAudit(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String): Int =
  {
    val query = new CompositeData[ AuditLogItem ](new Connection(), new AuditLogItem());
    countComposite(ui, cnxn, agentSessionId, tag, query)
  }

  def countComposite(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: CompositeData[ _ <: Data ]): Int =
  {
    @volatile var count = 0
    val tagUnique = tag + UUID.randomUUID().toString
    ui.addListener(agentSessionId, UUID.randomUUID().toString, new MessageEventAdapter(tagUnique)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        e.msg match {
          case x: GetContentResponse => {
            count = x.data.size
          }
          case _ => {}
        }
      }
    });

    val getReq = new GetContentRequest(new EventKey(agentSessionId, tagUnique), query)

    getReq.originCnxn = cnxn
    getReq.targetCnxn = cnxn
    ui.send(getReq)

    trySleep(count)
    count
  }

  def performOperationOnFetchedData(handler: (Data, AgentCnxn) => Unit,
    q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String) =
  {
    var actual: Data = null
    handler(fetchData(q, cnxn, key), cnxn)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Data =
    {
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: Data))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }


  def performAssertOperationOnFetchedData(handler: (Data, AgentCnxn) => Boolean, q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String) =
  {
    var actual: Data = null
    handler(fetchData(q, cnxn, key), cnxn) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)

    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Data =
    {
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: Data))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }

  def performOperationOnFetchedPost(handler: (Post, AgentCnxn, AgentCnxn) => Boolean, q: BasePlatformAgent with Storage, selfCnxn: AgentCnxn, directCnxn: AgentCnxn, key: String) =
  {
    var actual: Post = null

    println("attempting assert")

    handler(fetchPost(q, directCnxn, key), selfCnxn, directCnxn) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)

    def fetchPost(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Post =
    {
      println("BasePlatformAgent is = " + q)
      q.fetch[ Post ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: Post))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxn, data: Post) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }


  def fetchMustBe(handler: ( Post ) => Boolean)(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String) =
  {
    var actual: Post = null

    println("attempting assert")

    ( handler(fetchPost(q, cnxn, key)) ) must be_==(true).eventually(10, TIMEOUT_EVENTUALLY)

    def fetchPost(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Post =
    {
      println("BasePlatformAgent is = " + q)
      q.fetch[ Post ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: Post))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxn, data: Post) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }


  def fetchMustBe(expected: Data)(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String) =
  {
    var actual: Data = null

    println("attempting assert")

    fetchData(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)
    println("fetchMustBe expecting: " + expected)
    def fetchData(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Data =
    {
      println("BasePlatformAgent is = " + q)
      q.fetch[ Data ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: Data))
      trySleep(actual)
      println("----fetched data:" + actual)
      return actual
    }

    def handleFetch(cnxn: AgentCnxn, data: Data) =
    {
      println("-----getMustBe - get received : " + data)
      actual = data;
    }

  }

  def countMustBe(expected: Int)(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String) =
  {
    var found = 0

    println("attempting count")
    fetchCount(q, cnxn, key) must be_==(expected).eventually(5, TIMEOUT_EVENTUALLY)

    def fetchCount(q: BasePlatformAgent with Storage, cnxn: AgentCnxn, key: String): Int =
    {
      q.fetchList[ Any ](q._dbQ, cnxn, key, handleFetch(_: AgentCnxn, _: List[ Any ]))
      trySleep(found)
      return found
    }

    def handleFetch(cnxn: AgentCnxn, data: List[ Any ]) =
    {
      println("getMustBe - get received : " + data)
      for ( e <- data ) {
        println("*************fetchCount - received : " + e.toString)
        if ( e.toString != "" )
          found += 1
      }
    }
  }

  def countConnectionsByCategory(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: Data, category: String): Int =
  {
    countConnectionsByComparison(ui, cnxn, agentSessionId, tag, query, compareConnectionCategory(_: Connection, category))
  }

  def compareConnectionCategory(actual: Connection, expectedCategory: String): Boolean =
  {
    actual.category.equals(expectedCategory)
  }


  def countConnectionsByType(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: Data, connectionType: String): Int =
  {
    countConnectionsByComparison(ui, cnxn, agentSessionId, tag, query, compareConnectionType(_: Connection, connectionType))
  }

  def compareConnectionType(actual: Connection, expectedConnectionType: String): Boolean =
  {
    actual.connectionType.equals(expectedConnectionType)
  }


  def countConnectionsByName(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: Data, connectionName: String): Int =
  {
    countConnectionsByComparison(ui, cnxn, agentSessionId, tag, query, compareConnectionName(_: Connection, connectionName))
  }

  def compareConnectionName(actual: Connection, expectedConnectionName: String): Boolean =
  {
    actual.alias.equals(expectedConnectionName)
  }


  def countConnectionsByComparison(ui: AgentHostUIPlatformAgent, cnxn: AgentCnxn, agentSessionId: UUID, tag: String, query: Data, compare: ( Connection ) => Boolean): Int =
  {
    val sync = new AnyRef()
    //tag needs to be random otherwise only the 1st listen will wake up by the time the 4th listen is applying the must be_==
    //we intend to do many separate listens
    @volatile var count = 0
    val tagUnique = tag + UUID.randomUUID().toString
    ui.addListener(agentSessionId, "", new MessageEventAdapter(tagUnique)
    {
      override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
      {
        println("===========================getContentResponseReceived: " + e)
        e.msg match {
          case x: GetContentResponse => {
            sync.synchronized {
              count = x.data.size
              x.data.foreach(response => {
                if ( count != -999 ) {
                  response match {
                    case x: Connection => {
                      println("connection found=" + x)
                      if ( !compare(x) ) {
                        count = -999 // Error
                      }
                    }
                    case _ => {
                      count = -999 // also something odd
                    }
                  }
                }
              }
              )
            }
          }
          case _ => {}
        }
      }
    });

    val getReq = new GetContentRequest(new EventKey(agentSessionId, tagUnique), query)
    getReq.originCnxn = cnxn
    getReq.targetCnxn = cnxn
    ui.send(getReq)

    trySleep(sync.synchronized {count})
    sync.synchronized {count}
  }

}
