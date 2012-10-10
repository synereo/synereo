package com.protegra_ati.agentservices.core.messages.content

/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.OptionExtensions._
import actors.threadpool.LinkedBlockingQueue
import net.lag._
import com.protegra_ati.agentservices.core.schema._
import disclosure._
import org.junit._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.AgentTS.mTT._
import moniker._
import scala.util.continuations._
import com.protegra_ati.agentservices.core.messages._
import scala.util.Random
import java.net.URI
import org.specs.runner._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._

import events.{MessageEventAdapter, GetContentResponseReceivedEvent}
import platformagents._
import scala.collection.JavaConversions._
import java.util.{Locale, UUID}
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.util.serializer.Serializer

class ContentRequestSetTest
  extends JUnit4(ContentRequestSetTestSpecs)

object ContentRequestSetTestSpecsRunner
  extends ConsoleRunner(ContentRequestSetTestSpecs)

object ContentRequestSetTestSpecs extends Specification
with RabbitTestSetup
with Timeouts
with SpecsPAHelpers
{
  var cnxnUIStore = new AgentCnxnProxy("UI".toURI, "", "Store".toURI)
  val eventKey = "content"
  //val pa = new AgentHostStorePlatformAgent()
  //Profile
  val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
  val authorizedProfileContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
  val authorizedProfileContentIntroduced = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Introduced)
  val authorizedProfileContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

  // Optimization, prevents repeated execution of store initialization
  AgentHostCombinedBase.setup(this)
  //  val ui = AgentHostCombinedBase.uiRef
  val pa = AgentHostCombinedBase.storeRef

  "setContentByConnectionType" should {
   // skip("")
    val JenId = ( "Jen" + UUID.randomUUID )
    val MikeId = ( "Mike" + UUID.randomUUID )
    val cnxn = new AgentCnxnProxy(JenId.toURI, "", JenId.toURI)
    val profileId = UUID.randomUUID
    val mockProfile = new Profile("FirstName", "LastName", "", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
    val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")

    val jensCnxn = ConnectionFactory.createConnection("alias", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "connectionType", JenId, JenId)
    val connMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", JenId, MikeId)


    pa.store(pa._dbQ, pa._storeCnxn, jensCnxn.toStoreKey, Serializer.serialize[ Connection ](jensCnxn))
    Thread.sleep(TIMEOUT_SHORT)
    // set app id to self
    val appId = setAppId(cnxn)

    pa.store(pa._dbQ, cnxn, connMike.toStoreKey, Serializer.serialize[ Connection ](connMike))
    Thread.sleep(TIMEOUT_MED)

    "store content for all connections of the connection type" in {
    //skip("")
      // store Profile
      pa.setContentByConnectionType(cnxn, mockProfile, mockProfile, authorizedProfileContentBasic)
      Thread.sleep(TIMEOUT_SHORT)

      //check that the Profile saved to MikeJen has a proper values according to applied disclosed data
      val profileSearch: Profile = new Profile()
      fetchMustBe(basicProfile)(pa, connMike.writeCnxn, profileSearch.toSearchKey)
      // simple hashcode check
      val image: Image = new Image("name", "contentType", null, "metadata")
      val hashcode1 = image.hashCode
      image.id = "bubu"
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
    //skip("")
    val authorizedProfileContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
    //    val authorizedBusinessContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
    val authorizedProfileContentIntroduced = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Introduced)
    val authorizedProfileContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

    val profileId = UUID.randomUUID
    val mockProfile = new Profile("JenFirst", "JenLast", "test Description", "firstLast@test.com", "CA", "someprovince", "city", "postalCode", "website")
    val emptyProfile = new Profile()
    val introducedProfile = new Profile("", "JenLast", "", "", "CA", "", "", "", "")
    val fullProfile = mockProfile.copy()
    val reducedFullProfile = mockProfile.copy()
    reducedFullProfile.website = ""
    reducedFullProfile.region = ""
    reducedFullProfile.image = new Image()

    val JenId = ( "Jen" + UUID.randomUUID )
    val MikeId = ( "Mike" + UUID.randomUUID )
    val SteveId = ( "Steve" + UUID.randomUUID )

    val cnxnJenSelf = new AgentCnxnProxy(JenId.toURI, "", JenId.toURI)
    val connMikeIntroduced = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Introduced", JenId, MikeId)
    val connSteveFull = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", JenId, SteveId)
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
    val req = SetContentRequest(new EventKey(UUID.randomUUID, ""), mockProfile, null)
    req.targetCnxn = cnxnJenSelf
    req.originCnxn = cnxnJenSelf
    pa.processSetContentRequest(req)
    Thread.sleep(TIMEOUT_LONG)

    "save full object to own connection" in {
      // skip("")
      fetchMustBe(fullProfile)(pa, cnxnJenSelf, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure" in {
      // skip("")
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
    }


    "save object to all connections with Introduced disclosure" in {
      // skip("")
      fetchMustBe(introducedProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Introduced disclosure then upgrade to Full disclosure" in {
      // skip("")
      fetchMustBe(introducedProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
      val connMikeFull = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", JenId, MikeId)
      //create a setcontentrequest message  to upgrade the conn. level
      val reqFull = SetContentRequest(new EventKey(UUID.randomUUID, ""), connMikeFull, connMikeIntroduced)
      reqFull.targetCnxn = cnxnJenSelf
      reqFull.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqFull)

      // check profile & business profile
      fetchMustExclusivelyBe(fullProfile)(pa, connMikeIntroduced.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure then downgrade to Introduced disclosure" in {
      //skip("")
      fetchMustExclusivelyBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      val connSteveIntroduced = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, TrustLevel.Introduced.toString, JenId, SteveId)
      System.err.println("introduced connection: " + connSteveIntroduced)
      System.err.println("introduced full: " + connSteveFull)
      System.err.println("self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrate to introduced
      val reqIntroduced = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveIntroduced, connSteveFull)
      reqIntroduced.targetCnxn = cnxnJenSelf
      reqIntroduced.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqIntroduced)
      System.err.println("-------------------------------------------------------------------------")
      Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile & business profile
      fetchMustExclusivelyBe(introducedProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
    }

    "save object to all connections with Full disclosure then downgrade to Empty disclosure" in {
      // skip("")
      //check that the profile saved to AliceJen has no blue cross number
      fetchMustBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)

      val connSteveEmpty = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Empty", JenId, SteveId)
      //create a setcontentrequest message
      val req = SetContentRequest(new EventKey(UUID.randomUUID, ""), connSteveEmpty, connSteveFull)
      req.targetCnxn = cnxnJenSelf
      req.originCnxn = cnxnJenSelf

      pa.processSetContentRequest(req)
      fetchMustBe(emptyProfile)(pa, connSteveEmpty.writeCnxn, queryProfile.toSearchKey)
    }


    "change full disclosure by profile reducing number of fields applied to all connections" in {
   //skip("")
      fetchMustExclusivelyBe(fullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      countMustBe(1)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      // fields: website and image are removed
      val fullProfileNewDisclosedData = DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Full.toString, "id,localeCode,firstName,lastName,description,emailAddress,country,city,postalCode")
      val fullProfileOldDisclosedData = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

      System.err.println("IN TEST self conn:" + cnxnJenSelf)
      //create a setcontentrequest message to downgrate to introduced
      val reqFullDisclosedDataChanged = SetContentRequest(new EventKey(UUID.randomUUID, ""), fullProfileNewDisclosedData, fullProfileOldDisclosedData)
      reqFullDisclosedDataChanged.targetCnxn = cnxnJenSelf
      reqFullDisclosedDataChanged.originCnxn = cnxnJenSelf
      pa.processSetContentRequest(reqFullDisclosedDataChanged)
      System.err.println("-------------------------------------------------------------------------")
      Thread.sleep(TIMEOUT_VERY_LONG * 3)
      Thread.sleep(TIMEOUT_VERY_LONG * 3)
      // check profile if image and webseite are deleted
      countMustBe(1)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      fetchMustBeWithHandler(expectationCheckHandler(_: Data, _: Data))(reducedFullProfile, pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)
      fetchMustExclusivelyBe(reducedFullProfile)(pa, connSteveFull.writeCnxn, queryProfile.toSearchKey)

      def expectationCheckHandler(result: Data, expected: Data): Unit =
      {
        if ( result != expected ) fail()
        System.err.println("%%%%%%%%%%%%%%%%%%%%%%%% result=" + result + ", expected=" + expected)
      }
    }
  }

  "processGetContentRequest" should {
    //skip("")
    val profileId = UUID.randomUUID
    val mockProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
    val noProfile = new Profile("", "", "", "", "", "", "", "", "")
    val basicProfile = new Profile("FirstName", "LastName", "", "", "CA", "", "", "", "")
    val fullProfile = new Profile("FirstName", "LastName", "managerProfile", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

    val JenId = ( "Jen" + UUID.randomUUID )
    val MikeId = ( "Mike" + UUID.randomUUID )
    val SteveId = ( "Steve" + UUID.randomUUID )

    val cnxn = new AgentCnxnProxy(JenId.toURI, "", JenId.toURI)
    val connMikeBasic = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", JenId, MikeId)
    val connSteveFull = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", JenId, SteveId)

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

    "save an AuditLogItem for the request" in {
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
   // skip ("")
    val newUserId = ( "NewUser" + UUID.randomUUID )
    val connAdmin = ConnectionFactory.createConnection("admin", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "admin", newUserId, newUserId)
    val req = SetContentAdminRequest(null, connAdmin, null)
    req.originCnxn = cnxnUIStore

    pa.processSetContentAdminRequest(req)

    "save object to the store self connection" in {
      val connectionSearch: Connection = new Connection()
      //      pa.fetch[ Data ](pa._dbQ, pa._storeCnxn, connectionSearch.toSearchKey, handleFetchConnection)
      fetchMustBe(connAdmin)(pa, pa._storeCnxn, connectionSearch.toSearchKey)
    }

  }

  "processDeleteContentRequest for Connection" should {
    //skip("")
    val JenId = ( "Jen" + UUID.randomUUID )
    val MikeId = ( "Mike" + UUID.randomUUID )

    val jenSelfCnxn = new AgentCnxnProxy(JenId.toURI, "", JenId.toURI)
    val connJenMike = ConnectionFactory.createConnection("Mike", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Full", JenId, MikeId)
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
    val req = DeleteContentRequest(null, connectionSearch)
    req.targetCnxn = jenSelfCnxn
    req.originCnxn = cnxnUIStore

    pa.processDeleteContentRequest(req)
    Thread.sleep(TIMEOUT_LONG)

    var resultProfile: Option[ Profile ] = None
    var resultConnection: Option[ Connection ] = None

    "delete the correct connection from the connection list" in {
      //make sure that the Jen/Mike Connection is gone from the Jen/Jen collection
      fetchConnectionData(pa._dbQ, jenSelfCnxn, connectionSearch.toSearchKey) must be_==(None).eventually(20, TIMEOUT_EVENTUALLY)
    }

    "drop the store collection" in {
      val profileSearch: Profile = new Profile()

      //make sure that the Jen/Mike data is gone
      fetchProfileData(pa._dbQ, connJenMike.writeCnxn, profileSearch.toSearchKey) must be_==(None).eventually(20, TIMEOUT_EVENTUALLY)
    }

    def fetchProfileData(queue: PartitionedStringMGJ, cnxn: AgentCnxnProxy, key: String): Option[ Data ] =
    {
      resultProfile = None
      pa.fetch[ Data ](queue, cnxn, key, handleProfileFetch)
      return resultProfile
    }

    def fetchConnectionData(queue: PartitionedStringMGJ, cnxn: AgentCnxnProxy, key: String): Option[ Data ] =
    {
      resultConnection = None
      pa.fetch[ Data ](queue, cnxn, key, handleConnectionFetch)
      return resultConnection
    }

    def handleProfileFetch(cnxn: AgentCnxnProxy, data: Data) =
    {
      data match {
        case x: Profile => resultProfile = Some(x)
        case _ => None
      }
    }

    def handleConnectionFetch(cnxn: AgentCnxnProxy, data: Data) =
    {
      data match {
        case x: Connection => resultConnection = Some(x)
        case _ => None
      }
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
    val appId = AppId("TestApp")
    setContentToSelfConnection(self, appId)
    appId
  }
}


