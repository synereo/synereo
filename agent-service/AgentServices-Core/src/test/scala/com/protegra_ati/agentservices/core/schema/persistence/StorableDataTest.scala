package com.protegra_ati.agentservices.core.schema.persistence

import org.junit._
import Assert._
import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import java.util.UUID
import java.util.Date
import java.util.Locale
import java.util.Properties
import java.io.Writer
import java.io.FileWriter
import java.io.IOException
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.behaviors.Tracking
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra_ati.agentservices.core.schema.util._
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.messages.invitation.CreateInvitationRequest
import org.joda.time.DateTime

class StorableDataTest
  extends JUnit4(StorableDataTestSpecs)

object StorableDataTestSpecsRunner
  extends ConsoleRunner(StorableDataTestSpecs)

object StorableDataTestSpecs extends Specification
{
  "store key" should {
    val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString

    val conn = new MockConnection
    conn.id = id
    val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
    val NULL_DATA_TIME = "1980-01-01T00:00:00.000-00:00"
    //val created = new DateTime(NULL_DATA_TIME)

   // val connection = new Connection("business", "connectionType", "alias", null, null, "autoApprove", List(ConnectionPolicy.ReferralDisabled, ConnectionPolicy.SearchDisabled),created)

    val connection = ConnectionFactory.createConnection("alias", ConnectionCategory.Business.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)

    connection.id = id

    "generate store key correctly for Data" in {
      val storeKey = conn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      storeKey must be_==("mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\")))")
    }

    //Connection needs to be updated to output the right key again
    "generate store key correctly for Connection" in {
      val storeKey = connection.toStoreKey
      println("storeKey: " + storeKey)
      val expected= ("connection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),category(\"self\"),connectionType(\"connectionType\"),alias(\"alias\"),readCnxn(\"\"),writeCnxn(\"\"),autoApprove(\"true\"),policies(\"[]\"),created(\""+connection.created+"\")))")
      println("expected: " + expected)
      storeKey must be_==(expected)
    }

    "generate store key correctly for french Data" in {
      conn.setLocaleCode(Locale.FRENCH.toString())
      val storeKey = conn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      storeKey must be_==("mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"fr\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\")))")
    }

    "generate store key correctly for a System Data" in {
      val systemData = new SystemData[ MockConnection ](conn)
      val storeKey = systemData.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      storeKey must be_==("systemData(mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"))))")
    }

    "generate store key correctly for a MockProfile" in {
      val idN = "MY_ID"
      val mockProfile = new MockProfile("FirstName", "LastName", "", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      mockProfile.image = new Image("name", "", "content hasn't be visible in a toStoreKey", "meta1 meta2 meta3")
      mockProfile.id = idN
      mockProfile.localeCode = "en"
      val storeKey = mockProfile.toStoreKey
      val expectedStoreKey = "mockProfile(fields(id(\"" + idN + "\"),localeCode(\"en\"),firstName(\"FirstName\"),lastName(\"LastName\"),description(\"\"),emailAddress(\"123456789@test.com\"),country(\"CA\"),region(\"someCAprovince\"),city(\"city\"),postalCode(\"postalCode\"),website(\"website\"),image(\"\")))"
//      System.err.println("created  storeKey4MockProfile: " + storeKey)
//      System.err.println("expected storeKey4MockProfile: " + expectedStoreKey)
      storeKey must be_==(expectedStoreKey)
    }


    "generate store key correctly for Data with Tracking" in {
      val trackedConn = new MockConnectionTracked
      trackedConn.id = id

      val storeKey = trackedConn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      val expected = "mockConnectionTracked(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"),sent(\"\"),delivered(\"\"),viewed(\"\")))"
      storeKey must be_==(expected)
    }

    "generate store key correctly for Data with a Message and a Hashmap" in {
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val tempMockProfile = new MockProfile();
      tempMockProfile.setFirstName("first");
      tempMockProfile.setLastName("last");
      fromDetails.put(tempMockProfile.formattedClassName, tempMockProfile);

      val postToTarget = new Post("subject2Target", "body", fromDetails);
      val postToBroker = new Post("subject2Broker", "body", fromDetails);
      val msg = new CreateInvitationRequest(null, "", "", "", "", "", "", postToTarget, postToBroker);
      val persistedMessage = new PersistedMessage[ CreateInvitationRequest ](msg)
      persistedMessage.id = id

      val storeKey = persistedMessage.toStoreKey
      println("storeKey: " + storeKey)
      //      persistedMessage(fields(id("ec149c51-88f8-476c-bda7-8dd721d14a3f"),localeCode("en"),message(""),persisted("2012-05-02T22:01:02.903-05:00")))
      val expected = "persistedMessage(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),message(\"\"),persisted(\"" + persistedMessage.persisted + "\"),rejected(\"\"),ignored(\"\"),archived(\"\")))"
      storeKey must be_==(expected)
    }

    "generate store key correctly for an Image" in {
      val data: Image = new Image("name", "type", "content hasn't be visible in a toStoreKey", "myMetadata")
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      data.id = id
      val given = data.toStoreKey
      val expected = "image(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"name\"),contentType(\"type\"),content(\"\"),metadata(\"myMetadata\")))"
//      System.err.println("given:" + given)
//      System.err.println("expected:" + expected)
      given must be_==(expected)
    }
  }
}
