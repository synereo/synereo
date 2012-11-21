package com.protegra_ati.agentservices.core.messages.content

import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.OptionExtensions._
import actors.threadpool.LinkedBlockingQueue
import net.lag._
import com.protegra_ati.agentservices.core.schema._
import disclosure._
import org.junit._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import moniker._
import scala.util.continuations._
import com.protegra_ati.agentservices.core.messages._
import scala.util.Random
import java.net.URI

import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._

import events.{MessageEventAdapter, GetContentResponseReceivedEvent}
import platformagents._
import scala.collection.JavaConversions._
import java.util.{Locale, UUID}
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import org.specs2.specification.Scope


class ContentSetup(pa: AgentHostStorePlatformAgent) extends Scope
with RabbitTestSetup
with Timeouts
with Serializable
{

  val eventKey = "content"
  //Profile
  val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
  val authorizedProfileContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
  val authorizedProfileContentIntroduced = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Introduced)
  val authorizedProfileContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

  val cnxnUIStore = pa._cnxnUIStore

  val profileId = UUID.randomUUID
  val mockProfile = new Profile("JenFirst", "JenLast", "test Description", "firstLast@test.com", "CA", "someprovince", "city", "postalCode", "website")
  val emptyProfile = new Profile()
  val introducedProfile = new Profile("", "JenLast", "", "", "CA", "", "", "", "")
  val fullProfile = mockProfile.copy()
  val reducedFullProfile = mockProfile.copy()
  reducedFullProfile.website = ""
  reducedFullProfile.region = ""
  reducedFullProfile.image = new Image()

  val jenId = ( "Jen" + UUID.randomUUID )
  val mikeId = ( "Mike" + UUID.randomUUID )
  val steveId = ( "Steve" + UUID.randomUUID )

  val cnxnJenSelf = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
  val connMikeIntroduced = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Introduced", jenId, mikeId)
  val connSteveFull = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", jenId, steveId)
  pa.addToHostedCnxn(cnxnJenSelf)


  //save profile & business profile authorized content for empty, basic and full
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentEmpty.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentEmpty))
  Thread.sleep(TIMEOUT_LONG)
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentIntroduced.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentIntroduced))
  Thread.sleep(TIMEOUT_LONG)
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentFull.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentFull))

  // initial stat basic
  pa.put(pa._dbQ, cnxnJenSelf, connMikeIntroduced.toStoreKey, Serializer.serialize[ Data ](connMikeIntroduced))
  Thread.sleep(TIMEOUT_SHORT)
  // initial state full
  pa.put(pa._dbQ, cnxnJenSelf, connSteveFull.toStoreKey, Serializer.serialize[ Data ](connSteveFull))
  Thread.sleep(TIMEOUT_LONG)

  //create a setcontentrequest messages (save profile)
  // profile
  val queryProfile: Profile = new Profile()
  val setProfileRequest = SetContentRequest(new EventKey(UUID.randomUUID, ""), mockProfile, null)
  setProfileRequest.targetCnxn = cnxnJenSelf
  setProfileRequest.originCnxn = cnxnJenSelf
  pa.processSetContentRequest(setProfileRequest)
  Thread.sleep(TIMEOUT_LONG)
}

class DeleteSetup(pa: AgentHostStorePlatformAgent) extends Scope
with RabbitTestSetup
with Timeouts
with Serializable
{
  val cnxnUIStore = pa._cnxnUIStore

  val jenId = ( "Jen" + UUID.randomUUID )
  val mikeId = ( "Mike" + UUID.randomUUID )

  val jenSelfCnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
  val connJenMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", jenId, mikeId)
  pa.addToHostedCnxn(jenSelfCnxn)

  //store the connections
  pa.store(pa._dbQ, jenSelfCnxn, connJenMike.toStoreKey, Serializer.serialize[ Data ](connJenMike))
  //    Thread.sleep(TIMEOUT_LONG)

  //store data on the connections so that the collections are created
  val profile = new Profile("firstName", "lastName", "", "999999999@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
  pa.store(pa._dbQ, connJenMike.writeCnxn, profile.toStoreKey, Serializer.serialize[ Data ](profile))
  Thread.sleep(TIMEOUT_LONG)

  //delete the Jen/Mike connection
  val connectionSearch: Connection = new Connection()
  connectionSearch.id = connJenMike.id.toString
  val req = DeleteContentRequest(new EventKey(UUID.randomUUID(), "tag"), connectionSearch)
  req.targetCnxn = jenSelfCnxn
  req.originCnxn = cnxnUIStore

  pa.processDeleteContentRequest(req)
  Thread.sleep(TIMEOUT_LONG)

  var resultProfile: Option[ Profile ] = None
  var resultConnection: Option[ Connection ] = None
}

class ContentRequestSetTest extends SpecificationWithJUnit
with RabbitTestSetup
with Timeouts
with SpecsPAHelpers
with Serializable
{
  sequential

  // Optimization, prevents repeated execution of store initialization
  val cnxnUIStore = new AgentCnxnProxy(( "UI" + UUID.randomUUID().toString ).toURI, "", ( "Store" + UUID.randomUUID().toString ).toURI)
  val pa = new AgentHostStorePlatformAgent()
  AgentHostCombinedBase.setupStore(pa, cnxnUIStore)

  "setContentByConnectionType" should {

    "store content for all connections of the connection type" in new ContentSetup(pa)
    {
      val cnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
      val profileId1 = UUID.randomUUID
      val profile = new Profile("FirstName", "LastName", "", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")

      val jensCnxn = ConnectionFactory.createConnection("alias", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "connectionType", jenId, jenId)
      val connMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", jenId, mikeId)


      pa.store(pa._dbQ, pa._storeCnxn, jensCnxn.toStoreKey, Serializer.serialize[ Connection ](jensCnxn))
      Thread.sleep(TIMEOUT_SHORT)
      // set app id to self
      val appId = setAppId(cnxn)

      pa.store(pa._dbQ, cnxn, connMike.toStoreKey, Serializer.serialize[ Connection ](connMike))
      Thread.sleep(TIMEOUT_MED)

      // store Profile
      pa.setContentByConnectionType(cnxn, profile, profile, authorizedProfileContentBasic)
      Thread.sleep(TIMEOUT_SHORT)

      //check that the Profile saved to MikeJen has a proper values according to applied disclosed data
      val profileSearch: Profile = new Profile()
      fetchMustBe(basicProfile)(pa, connMike.writeCnxn, profileSearch.toSearchKey)
      // simple hashcode check
      val image: Image = new Image("name", "contentType", null.asInstanceOf[ String ], "metadata")
      val hashcode1 = image.hashCode
      image.id = "temp"
      ( hashcode1 ) must be_==(image.hashCode)

      //check that the appropriate DisclosedData object was persisted to the connMike.writeCnxn
      //  val authorizedContentAuditItemSearch: AuthorizedContentAuditItem = new AuthorizedContentAuditItem(basicAuditItem.objectType, basicAuditItem.connectionType, "", "")
      val basicAuditItem = authorizedProfileContentBasic.forAudit(connMike)
      val authorizedContentAuditItemSearch: AuthorizedContentAuditItem = new AuthorizedContentAuditItem()
      authorizedContentAuditItemSearch.objectType = basicAuditItem.objectType
      authorizedContentAuditItemSearch.connectionType = basicAuditItem.connectionType

      //authorizedContentAuditItemSearch.setSearchFieldValue("objectType", basicAuditItem.objectType)
      //authorizedContentAuditItemSearch.setSearchFieldValue("connectionType", basicAuditItem.connectionType)

      val comparable = authorizedProfileContentBasic.forAudit(connMike)
      fetchMustBe(comparable)(pa, connMike.writeCnxn, authorizedContentAuditItemSearch.toSearchKey)
    }

  }

  "processSetContentRequest with Data" should {

    "save full object to own connection" in new ContentSetup(pa)
    {
      fetchMustBe(fullProfile)(pa, cnxnJenSelf, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
    }


    "save object to all connections with Introduced disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(introducedProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Introduced disclosure then upgrade to Full disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(introducedProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
      val connMikeFull = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", jenId, mikeId)
      //create a setcontentrequest message  to upgrade the conn. level
      val reqFull = SetContentRequest(new EventKey(UUID.randomUUID, ""), connMikeFull, connMikeIntroduced)
      reqFull.targetCnxn = cnxnJenSelf
      reqFull.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqFull)

      // check profile & business profile
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(fullProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
      //      fetchMustExclusivelyBe(fullProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure then downgrade to Introduced disclosure" in new ContentSetup(pa)
    {
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      //          fetchMustExclusivelyBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      val connSteveIntroduced = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Introduced.toString, jenId, steveId)
      println("introduced connection: " + connSteveIntroduced)
      println("introduced full: " + connSteveFull)
      println("self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrate to introduced
      val reqIntroduced = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveIntroduced, connSteveFull)
      reqIntroduced.targetCnxn = cnxnJenSelf
      reqIntroduced.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqIntroduced)
      println("-------------------------------------------------------------------------")
      //          Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile & business profile
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(introducedProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      //          fetchMustExclusivelyBe(introducedProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure then downgrade to Empty disclosure" in new ContentSetup(pa)
    {
      //check that the profile saved to AliceJen has no blue cross number
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)

      val connSteveEmpty = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Empty", jenId, steveId)
      //create a setcontentrequest message
      val req = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveEmpty, connSteveFull)
      req.targetCnxn = cnxnJenSelf
      req.originCnxn = cnxnJenSelf

      pa.processSetContentRequest(req)
      fetchMustBe(emptyProfile)(pa, connSteveEmpty.writeCnxn, queryProfile.toSearchKey)
    }
    "change full disclosure by profile reducing number of fields applied to all connections" in new ContentSetup(pa)
    {
      //skipped("")
         //TODO:fetchMustExclusivelyBe
//      fetchMustExclusivelyBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      countMustBe(1)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      // fields: website and image are removed
      val fullProfileNewDisclosedData = DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Full.toString, "id,localeCode,firstName,lastName,description,emailAddress,country,city,postalCode")
      val fullProfileOldDisclosedData = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

      println("IN TEST self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrate to introduced
      val reqFullDisclosedDataChanged = SetContentRequest(new EventKey(UUID.randomUUID, ""), fullProfileNewDisclosedData, fullProfileOldDisclosedData)
      reqFullDisclosedDataChanged.targetCnxn = cnxnJenSelf
      reqFullDisclosedDataChanged.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqFullDisclosedDataChanged)
      println("-------------------------------------------------------------------------")
      Thread.sleep(TIMEOUT_VERY_LONG)
//      Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile if image and webseite are deleted
      countMustBe(1)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      fetchMustBeWithHandler(expectationCheckHandler(_: Data, _: Data))(reducedFullProfile, pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
           //TODO:fetchMustExclusivelyBe
//      fetchMustExclusivelyBe(reducedFullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(reducedFullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)

      def expectationCheckHandler(result: Data, expected: Data): Unit =
      {
        if ( result != expected ) failure
        println("%%%%%%%%%%%%%%%%%%%%%%%% result=" + result + ", expected=" + expected)
      }
    }
  }

    "processGetContentRequest" should {

      "save an AuditLogItem for the request" in {
        val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
        val authorizedProfileContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
        val authorizedProfileContentIntroduced = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Introduced)
        val authorizedProfileContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

        val cnxnUIStore = pa._cnxnUIStore

        val profileId = UUID.randomUUID
        val mockProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
        val noProfile = new Profile("", "", "", "", "", "", "", "", "")
        val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")
        val fullProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

        val jenId = ( "Jen" + UUID.randomUUID )
        val mikeId = ( "Mike" + UUID.randomUUID )
        val steveId = ( "Steve" + UUID.randomUUID )

        val cnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
        val connMikeBasic = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", jenId, mikeId)
        val connSteveFull = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", jenId, steveId)

        //save profile authorized content for basic and full
        pa.store(pa._dbQ, cnxn, authorizedProfileContentEmpty.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentEmpty))
        pa.store(pa._dbQ, cnxn, authorizedProfileContentBasic.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentBasic))
        pa.store(pa._dbQ, cnxn, authorizedProfileContentFull.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentFull))

        pa.store(pa._dbQ, cnxn, connMikeBasic.toStoreKey, Serializer.serialize[ Data ](connMikeBasic))
        pa.store(pa._dbQ, cnxn, connSteveFull.toStoreKey, Serializer.serialize[ Data ](connSteveFull))
        Thread.sleep(TIMEOUT_LONG)

        //create a setcontentrequest message
        val req = SetContentRequest(new EventKey(UUID.randomUUID, ""), mockProfile, null)
        req.targetCnxn = cnxn
        req.originCnxn = cnxn
        pa.processSetContentRequest(req)
        Thread.sleep(TIMEOUT_LONG)
        //setup is complete....requesting content

        val profileSearch: Profile = new Profile()
        val getReq = GetContentRequest(new EventKey(UUID.randomUUID, ""), profileSearch)
        getReq.targetCnxn = connMikeBasic.writeCnxn
        getReq.originCnxn = cnxnUIStore
        pa.processGetContentRequest(getReq)
        Thread.sleep(TIMEOUT_LONG)
        val auditSearch: AuditLogItem = new AuditLogItem()
        //check that the profile saved to AliceJen has no blue cross number
        countMustBe(1)(pa, connMikeBasic.writeCnxn, auditSearch.toSearchKey)
      }

    }

    "processSetContentAdminRequest" should {
      "save object to the store self connection" in {
        val cnxnUIStore = pa._cnxnUIStore
        val newUserId = ( "NewUser" + UUID.randomUUID )
        val connAdmin = ConnectionFactory.createConnection("admin", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "admin", newUserId, newUserId)
        val req = SetContentAdminRequest(null, connAdmin, null)
        req.originCnxn = cnxnUIStore

        pa.processSetContentAdminRequest(req)
        val connectionSearch: Connection = new Connection()
        //      pa.fetch[ Data ](pa._dbQ, pa._storeCnxn, connectionSearch.toSearchKey, handleFetchConnection)
        fetchMustBe(connAdmin)(pa, pa._storeCnxn, connectionSearch.toSearchKey)
      }

    }

    "processDeleteContentRequest for Connection" should {


      "delete the correct connection from the connection list" in new DeleteSetup(pa) {
        //make sure that the Jen/Mike Connection is gone from the Jen/Jen collection
        fetchMustBe(null)(pa, jenSelfCnxn, connectionSearch.toSearchKey)
      }

      "drop the store collection" in  new DeleteSetup(pa) {
        val profileSearch: Profile = new Profile()

        //make sure that the Jen/Mike data is gone
        fetchMustBe(null)(pa, connJenMike.writeCnxn, profileSearch.toSearchKey)
      }

    }


  def createSomeProfilesForTestFull(name: String, country: String): Profile =
  {
    new Profile(name, "last", "test Description", name + "@test.com", country, "someprovince", "city", "postalCode", "website")

  }

  def createSomeProfilesForTestFull(name: String, lastName: String, country: String): Profile =
  {
    new Profile(name, lastName, "test Description", name + "@test.com", country, "someprovince", "city", "postalCode", "website")
  }

  def profileOfLevel(srcProfile: Profile, level: String): Profile =
  {
    var result: Profile = null
    if ( "empty".equals(level.toLowerCase) ) result = new Profile()
    else if ( "basic".equals(level.toLowerCase) ) {
      result = srcProfile.copy(description = "", emailAddress = "", country = "", region = "", city = "", postalCode = "")
    }
    else if ( "full".equals(level.toLowerCase) ) {
      result = srcProfile.copy() // full copy
    }
    else if ( "introduced".equals(level.toLowerCase) ) {
      result = srcProfile.copy(firstName = "", description = "", emailAddress = "", country = "", region = "", city = "", postalCode = "")
    }

    else result = srcProfile.copy() // full copy
    result

  }

  def setContentToSelfConnection(cnxSelf: AgentCnxnProxy, data: Data) =
  {
    val msg = new SetContentRequest(new EventKey(UUID.randomUUID, ""), data, null)
    msg.targetCnxn = cnxSelf
    msg.originCnxn = cnxSelf
    pa.processSetContentRequest(msg)
    Thread.sleep(500)
  }

  def setAppId(self: AgentCnxnProxy): AppId =
  {
    val appId = new AppId("TestApp")
    setContentToSelfConnection(self, appId)
    appId
  }
}


