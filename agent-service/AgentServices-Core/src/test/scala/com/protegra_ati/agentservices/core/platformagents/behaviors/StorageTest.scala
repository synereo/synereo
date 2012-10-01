package com.protegra_ati.agentservices.core.platformagents.behaviors

/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

import org.specs._

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import moniker._
import com.protegra_ati.agentservices.core.messages._
import org.specs.runner._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import com.protegra_ati.agentservices.core.schema.disclosure._

import platformagents._
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.Serializer

class StorageTest
  extends JUnit4(StorageTestSpecs)

object StorageTestSpecsRunner
  extends ConsoleRunner(StorageTestSpecs)

object StorageTestSpecs extends Specification
with RabbitTestSetup
with Timeouts
with SpecsPAHelpers
{
  //var cnxnUIStore = new AgentCnxnProxy("UI".toURI, "", "Store".toURI)
  //var cnxnMike = new AgentCnxnProxy("Mike".toURI, "", "Mike".toURI)

  //val pa = new AgentHostStorePlatformAgent()

  val authorizedContentEmpty = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Empty)
  val authorizedContentBasic = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Basic)
  val authorizedContentFull = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Full)

  //  def createPA: AgentHostStorePlatformAgent =
  //  {
  //    val dbAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_DB)
  //    val privateAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
  //    val privateAcquaintanceAddresses = List[ URM ]()
  //    val publicAddress = "127.0.0.1".toURM.withPort(RABBIT_PORT_STORE_PUBLIC)
  //    val publicAcquaintanceAddresses = List[ URM ]()
  //    val pa = new AgentHostStorePlatformAgent()
  //    // pa._cnxnUIStore = cnxn
  //    pa.initForTest(publicAddress, publicAcquaintanceAddresses, privateAddress, privateAcquaintanceAddresses, dbAddress, UUID.randomUUID)
  //    Thread.sleep(TIMEOUT_LONG)
  //    pa
  //  }
  //
  //  val pa = createPA
  AgentHostCombinedBase.setup(this)

  val pa = AgentHostCombinedBase.storeRef

  "updateData" should {

    val ProfileId = UUID.randomUUID
    val mockProfile = new Profile("FirstName", "LastName", "", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
    val basicProfile = new Profile("FirstName", "LastName", "", "", "", "", "", "", "")

    val JenId = ( "Jen" + UUID.randomUUID )
    val SteveId = ( "Steve" + UUID.randomUUID )

    val connSteve = ConnectionFactory.createConnection("Steve", ConnectionCategory.Person.toString, ConnectionCategory.Person.toString, "Basic", JenId, SteveId)

    "insert new data" in {
      var oldData: Profile = null
      pa.updateData(connSteve.writeCnxn, mockProfile.authorizedData(authorizedContentBasic.fields), oldData)
      val ProfileSearch: Profile = new Profile()

      fetchMustBe(basicProfile)(pa, connSteve.writeCnxn, ProfileSearch.toSearchKey)
      countMustBe(1)(pa, connSteve.writeCnxn, ProfileSearch.toSearchKey)
    }

    "delete and insert existing data" in {
      val data = mockProfile.authorizedData(authorizedContentBasic.fields)
      pa.store(pa._dbQ, connSteve.writeCnxn, data.toStoreKey, Serializer.serialize[ Data ](data))
      Thread.sleep(TIMEOUT_MED)
      pa.updateData(connSteve.writeCnxn, data, data)
      val ProfileSearch: Profile = new Profile()

      fetchMustBe(basicProfile)(pa, connSteve.writeCnxn, ProfileSearch.toSearchKey)
      countMustBe(1)(pa, connSteve.writeCnxn, ProfileSearch.toSearchKey)
    }
  }
}
