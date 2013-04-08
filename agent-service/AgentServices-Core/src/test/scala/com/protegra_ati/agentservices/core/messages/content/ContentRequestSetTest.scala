package com.protegra_ati.agentservices.core.messages.content

import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.OptionExtensions._
import actors.threadpool.LinkedBlockingQueue
import net.lag._
import com.protegra_ati.agentservices.core.schema._
import disclosure._
import org.junit._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
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
with InitTestSetup
with Timeouts
with Serializable
{

  val eventKey = "content"
  //Profile
  val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
  val authorizedProfileContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
  val authorizedProfileContentCustom = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Custom)
  val authorizedProfileContentTrusted = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Trusted)

  val cnxnUIStore = pa._cnxnUIStore

  val profileId = UUID.randomUUID.toString
  val mockProfile = new Profile("JenFirst", "JenLast", "test Description", "firstLast@test.com", "CA", "someprovince", "city", "postalCode", "website")
  mockProfile.id = profileId
  val emptyProfile = new Profile()
  val customProfile = new Profile("JenFirst", "JenLast", "test Description", "", "CA", "someprovince", "city", "postalCode", "")
  customProfile.id = profileId
  val trustedProfile = mockProfile.copy()
  trustedProfile.id = profileId
  val reducedTrustedProfile = mockProfile.copy()
  reducedTrustedProfile.id = profileId
  reducedTrustedProfile.website = ""
  reducedTrustedProfile.region = ""
  reducedTrustedProfile.imageHashCode = ""

  val jenId = ( "Jen" + UUID.randomUUID )
  val mikeId = ( "Mike" + UUID.randomUUID )
  val steveId = ( "Steve" + UUID.randomUUID )

  val cnxnJenSelf = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
  val connMikeCustom = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Custom", jenId, mikeId)
  val connSteveTrusted = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Trusted", jenId, steveId)
  pa.addToHostedCnxn(cnxnJenSelf)


  //save profile & business profile authorized content for empty, custom and trusted
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentEmpty.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentEmpty))
  Thread.sleep(TIMEOUT_LONG)
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentCustom.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentCustom))
  Thread.sleep(TIMEOUT_LONG)
  pa.put(pa._dbQ, cnxnJenSelf, authorizedProfileContentTrusted.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentTrusted))

  // initial state custom and trusted
  pa.put(pa._dbQ, cnxnJenSelf, connMikeCustom.toStoreKey, Serializer.serialize[ Data ](connMikeCustom))
  Thread.sleep(TIMEOUT_SHORT)
  pa.put(pa._dbQ, cnxnJenSelf, connSteveTrusted.toStoreKey, Serializer.serialize[ Data ](connSteveTrusted))
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
with InitTestSetup
with Timeouts
with Serializable
{
  val cnxnUIStore = pa._cnxnUIStore

  val jenId = ( "Jen" + UUID.randomUUID )
  val mikeId = ( "Mike" + UUID.randomUUID )

  val jenSelfCnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
  val connJenMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Trusted", jenId, mikeId)
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

class SetContentAdminSetup(pa: AgentHostStorePlatformAgent) extends Scope
with InitTestSetup
with Timeouts
with Serializable
{
  pa._storeCnxn = new AgentCnxnProxy(( "Store" + UUID.randomUUID().toString ).toURI, "", ( "Self" + UUID.randomUUID().toString ).toURI)
}

class ContentRequestSetTest extends SpecificationWithJUnit
with InitTestSetup
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
      skipped("Not tracking audit data")
      val cnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
      val profileId1 = UUID.randomUUID
      val profile = new Profile("FirstName", "LastName", "", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")

      val jensCnxn = ConnectionFactory.createConnection("alias", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "connectionType", jenId, jenId)
      val connMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", jenId, mikeId)


      pa.store(pa._dbQ, pa._storeCnxn, jensCnxn.toStoreKey, Serializer.serialize(jensCnxn))
      Thread.sleep(TIMEOUT_SHORT)
      // set app id to self
      val appId = setAppId(cnxn)

      pa.store(pa._dbQ, cnxn, connMike.toStoreKey, Serializer.serialize(connMike))
      Thread.sleep(TIMEOUT_MED)

      // store Profile
      pa.setContentByConnectionType(cnxn, new Identification, new EventKey(), profile, profile, authorizedProfileContentBasic)
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
      //val basicAuditItem = authorizedProfileContentBasic.forAudit(connMike)
      //val authorizedContentAuditItemSearch: AuthorizedContentAuditItem = new AuthorizedContentAuditItem()
      //authorizedContentAuditItemSearch.objectType = basicAuditItem.objectType
      //authorizedContentAuditItemSearch.connectionType = basicAuditItem.connectionType

      //authorizedContentAuditItemSearch.setSearchFieldValue("objectType", basicAuditItem.objectType)
      //authorizedContentAuditItemSearch.setSearchFieldValue("connectionType", basicAuditItem.connectionType)

      //val comparable = authorizedProfileContentBasic.forAudit(connMike)
      //fetchMustBe(comparable)(pa, connMike.writeCnxn, authorizedContentAuditItemSearch.toSearchKey)
    }

  }

  "processSetContentRequest with Data" should {

    "save trusted object to own connection" in new ContentSetup(pa)
    {
      fetchMustBe(trustedProfile)(pa, cnxnJenSelf, queryProfile.toSearchKey)
    }

    "save object to all connections with Trusted disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
    }


    "save object to all connections with Custom disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(customProfile)(pa, connMikeCustom.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Custom disclosure then upgrade to Trusted disclosure" in new ContentSetup(pa)
    {
      fetchMustBe(customProfile)(pa, connMikeCustom.writeCnxn, queryProfile.toSearchKey)
      val connMikeTrusted = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Trusted", jenId, mikeId)
      connMikeTrusted.id = connMikeCustom.id
      var mike111: String = customProfile.toDeleteKey()
      var mike222: String = customProfile.toSearchKey
      //create a setcontentrequest message  to upgrade the conn. level
      val reqTrusted = SetContentRequest(new EventKey(UUID.randomUUID, ""), connMikeTrusted, connMikeCustom)
      reqTrusted.targetCnxn = cnxnJenSelf
      reqTrusted.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqTrusted)

      // check profile & business profile
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(trustedProfile)(pa, connMikeCustom.writeCnxn, queryProfile.toSearchKey)
      //      fetchMustExclusivelyBe(trustedProfile)(pa, connMikeCustom.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Trusted disclosure then downgrade to Custom disclosure" in new ContentSetup(pa)
    {
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      //          fetchMustExclusivelyBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      val connSteveCustom = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Custom.toString, jenId, steveId)
      connSteveCustom.id = connSteveTrusted.id
      println("custom connection: " + connSteveCustom)
      println("custom trusted: " + connSteveTrusted)
      println("self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrate to custom
      val reqCustom = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveCustom, connSteveTrusted)
      reqCustom.targetCnxn = cnxnJenSelf
      reqCustom.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqCustom)
      println("-------------------------------------------------------------------------")
      //          Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile & business profile
      //TODO:fetchMustExclusivelyBe
      fetchMustBe(customProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      //          fetchMustExclusivelyBe(customProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Trusted disclosure then downgrade to Empty disclosure" in new ContentSetup(pa)
    {
      //check that the profile saved to AliceJen has no blue cross number
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)

      val connSteveEmpty = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Empty", jenId, steveId)
      connSteveEmpty.id = connSteveTrusted.id
      //create a setcontentrequest message
      val req = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveEmpty, connSteveTrusted)
      req.targetCnxn = cnxnJenSelf
      req.originCnxn = cnxnJenSelf

      pa.processSetContentRequest(req)
      fetchMustBe(emptyProfile)(pa, connSteveEmpty.writeCnxn, queryProfile.toSearchKey)
    }
    "change trusted disclosure by profile reducing number of fields applied to all connections" in new ContentSetup(pa)
    {
      //skipped("")
         //TODO:fetchMustExclusivelyBe
//      fetchMustExclusivelyBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      countMustBe(1)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      // fields: website and image are removed
      val trustedProfileNewDisclosedData = DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Trusted.toString, "id,localeCode,firstName,lastName,description,emailAddress,country,city,postalCode")
      val trustedProfileOldDisclosedData = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Trusted)
      trustedProfileNewDisclosedData.id =  trustedProfileOldDisclosedData.id


      println("IN TEST self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrade to custom
      val reqTrustedDisclosedDataChanged = SetContentRequest(new EventKey(UUID.randomUUID, ""), trustedProfileNewDisclosedData, trustedProfileOldDisclosedData)
      reqTrustedDisclosedDataChanged.targetCnxn = cnxnJenSelf
      reqTrustedDisclosedDataChanged.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqTrustedDisclosedDataChanged)
      println("-------------------------------------------------------------------------")
      Thread.sleep(TIMEOUT_VERY_LONG)
//      Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile if image and webseite are deleted
      countMustBe(1)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      fetchMustBeWithHandler(expectationCheckHandler(_: Data, _: Data))(reducedTrustedProfile, pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
           //TODO:fetchMustExclusivelyBe
//      fetchMustExclusivelyBe(reducedTrustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(reducedTrustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)

      def expectationCheckHandler(result: Data, expected: Data): Unit =
      {
        if ( result != expected ) failure
        println("%%%%%%%%%%%%%%%%%%%%%%%% result=" + result + ", expected=" + expected)
      }
    }

    "not change trusted disclosure connections when reducing number of basic disclosure fields" in new ContentSetup(pa)
    {
      //TODO:fetchMustExclusivelyBe
      //fetchMustExclusivelyBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      countMustBe(1)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)

      // fields: lastName is removed
      val basicProfileNewDisclosedData = DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Basic.toString, "id,localeCode,firstName,description,country,imageHashCode")
      val basicProfileOldDisclosedData = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
      basicProfileNewDisclosedData.id =  basicProfileOldDisclosedData.id

      println("IN TEST self conn:" + cnxnJenSelf)

      //create a setcontentrequest message to remove basic fields
      val reqBasicDisclosedDataChanged = SetContentRequest(new EventKey(UUID.randomUUID, ""), basicProfileNewDisclosedData, basicProfileOldDisclosedData)
      reqBasicDisclosedDataChanged.targetCnxn = cnxnJenSelf
      reqBasicDisclosedDataChanged.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqBasicDisclosedDataChanged)
      println("-------------------------------------------------------------------------")
      Thread.sleep(TIMEOUT_VERY_LONG)

      //check profile that no changes were made
      fetchMustBeWithHandler(expectationCheckHandler(_: Data, _: Data))(trustedProfile, pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      //TODO:fetchMustExclusivelyBe
      //fetchMustExclusivelyBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      fetchMustBe(trustedProfile)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)
      countMustBe(1)(pa, connSteveTrusted.writeCnxn, queryProfile.toSearchKey)

      def expectationCheckHandler(result: Data, expected: Data): Unit =
      {
        if ( result != expected ) failure
        println("%%%%%%%%%%%%%%%%%%%%%%%% result=" + result + ", expected=" + expected)
      }
    }
  }

    "processGetContentRequest" should {

      "save an AuditLogItem for the request" in {
        skipped("")
        val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
        val authorizedProfileContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
        val authorizedProfileContentCustom = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Custom)
        val authorizedProfileContentTrusted = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Trusted)

        val cnxnUIStore = pa._cnxnUIStore

        val profileId = UUID.randomUUID
        val mockProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
        val noProfile = new Profile("", "", "", "", "", "", "", "", "")
        val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")
        val trustedProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

        val jenId = ( "Jen" + UUID.randomUUID )
        val mikeId = ( "Mike" + UUID.randomUUID )
        val steveId = ( "Steve" + UUID.randomUUID )

        val cnxn = new AgentCnxnProxy(jenId.toURI, "", jenId.toURI)
        val connMikeBasic = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", jenId, mikeId)
        val connSteveTrusted = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Trusted", jenId, steveId)

        //save profile authorized content for basic and trusted
        pa.store(pa._dbQ, cnxn, authorizedProfileContentEmpty.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentEmpty))
        pa.store(pa._dbQ, cnxn, authorizedProfileContentBasic.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentBasic))
        pa.store(pa._dbQ, cnxn, authorizedProfileContentTrusted.toStoreKey, Serializer.serialize[ Data ](authorizedProfileContentTrusted))

        pa.store(pa._dbQ, cnxn, connMikeBasic.toStoreKey, Serializer.serialize[ Data ](connMikeBasic))
        pa.store(pa._dbQ, cnxn, connSteveTrusted.toStoreKey, Serializer.serialize[ Data ](connSteveTrusted))
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
      "save object to the store self connection" in new SetContentAdminSetup(pa){
        val cnxnUIStore = pa._cnxnUIStore

        val newUserId = ( "NewUser" + UUID.randomUUID )
        val connAdmin = ConnectionFactory.createConnection("admin", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "admin", newUserId, newUserId)
        connAdmin.toStoreKey
        val req = SetContentAdminRequest(new EventKey(UUID.randomUUID, ""), connAdmin, null)
        req.originCnxn = cnxnUIStore

        pa.processSetContentAdminRequest(req)
        val connectionSearch: Connection = new Connection()
        connectionSearch.id = connAdmin.id
        Thread.sleep(1000)
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


  def createSomeProfilesForTestTrusted(name: String, country: String): Profile =
  {
    new Profile(name, "last", "test Description", name + "@test.com", country, "someprovince", "city", "postalCode", "website")

  }

  def createSomeProfilesForTestTrusted(name: String, lastName: String, country: String): Profile =
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
    else if ( "trusted".equals(level.toLowerCase) ) {
      result = srcProfile.copy() // trusted copy
    }
    else if ( "custom".equals(level.toLowerCase) ) {
      result = srcProfile.copy(firstName = "", description = "", emailAddress = "", country = "", region = "", city = "", postalCode = "")
    }

    else result = srcProfile.copy() // trusted copy
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


