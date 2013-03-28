package com.protegra_ati.agentservices.core.schema.persistence

import org.junit._
import Assert._
import org.specs2.mutable._
import org.junit.runner._
import org.specs2.runner._
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

class StorableDataTest extends SpecificationWithJUnit
{
  val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
  "store key" should {
    "generate store key correctly for Data" in {
      val conn = new MockConnection
      conn.id = id
      val storeKey = conn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      //      storeKey must be_==("mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\")))")
      storeKey must be_==("data(mockConnection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00 00 00\"),amt(\"1 2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0 0\"),test8(\"0 0\"),test9(\"\"),test10(\"\"))))")
    }

    //Connection needs to be updated to output the right key again
    "generate store key correctly for Connection" in {
     // skipped("TODO: need to update the test")

      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val connection = ConnectionFactory.createConnection("alias", ConnectionCategory.Business.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      connection.id = id

      val storeKey = connection.toStoreKey
      println("storeKey: " + storeKey)
      val expected = ( "data(connection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(category(\"Self\"),connectionType(\"connectionType\"),alias(\"alias\"),readCnxn(data(agentCnxnProxy(keys(id(\"\"),localeCode(\"\"),recVerNum(\"\")),fields(src(\"agent   Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"),label(\"\"),trgt(\"agent   Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"))))),writeCnxn(data(agentCnxnProxy(keys(id(\"\"),localeCode(\"\"),recVerNum(\"\")),fields(src(\"agent   Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"),label(\"\"),trgt(\"agent   Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"))))),autoApprove(\"true\"),policies(\"[]\"),created(\"" +  PrologFormatter.clean(connection.created.toString) + "\"))))" )
      println("expected: " + expected)
      storeKey must be_==(expected)
    }

    "generate store key correctly for french Data" in {
      val conn = new MockConnection
      conn.id = id
      conn.setLocaleCode(Locale.FRENCH.toString())
      val storeKey = conn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      //      storeKey must be_==("mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"fr\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\")))")
      storeKey must be_==("data(mockConnection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"fr\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00 00 00\"),amt(\"1 2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0 0\"),test8(\"0 0\"),test9(\"\"),test10(\"\"))))")
    }

    "generate store key correctly for a System Data" in {
      val conn = new MockConnection
      conn.id = id
      val systemData = new SystemData[ MockConnection ](conn)
      val storeKey = systemData.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      //      storeKey must be_==("systemData(mockConnection(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"))))")
      storeKey must be_==("systemData(data(mockConnection("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00 00 00\"),amt(\"1 2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0 0\"),test8(\"0 0\"),test9(\"\"),test10(\"\")))))")
    }

    "generate store key correctly for a Profile" in {
      val idN = "MY_ID"
      val mockProfile = new Profile("FirstName", "LastName?", "desc", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      mockProfile.imageHashCode = "1234"
      mockProfile.id = idN
      mockProfile.localeCode = "en"
      val storeKey = mockProfile.toStoreKey
      //      val expectedStoreKey = "profile(fields(id(\"" + idN + "\"),localeCode(\"en\"),firstName(\"FirstName\"),lastName(\"LastName\"),description(\"\"),emailAddress(\"123456789@test.com\"),country(\"CA\"),region(\"someCAprovince\"),city(\"city\"),postalCode(\"postalCode\"),website(\"website\"),image(\"\")))"
      val expectedStoreKey = "data(profile(" + KEYS + "(id(\"MY_ID\"),localeCode(\"en\"),recVerNum(\"1\")),fields(firstName(\"FirstName\"),lastName(\"LastName \"),emailAddress(\"123456789 test com\"),country(\"CA\"),region(\"someCAprovince\"),city(\"city\"),postalCode(\"postalCode\"),website(\"website\"),imageHashCode(\"1234\"))))"
      //      println("created  storeKey4Profile: " + storeKey)
      //      println("expected storeKey4Profile: " + expectedStoreKey)
      storeKey must be_==(expectedStoreKey)
    }


    "generate store key correctly for Data with Tracking" in {
      val trackedConn = new MockConnectionTracked
      trackedConn.id = id

      val storeKey = trackedConn.toStoreKey
      println("storeKey: " + storeKey)
      //      assertEquals("mockConnection(\"Jennifer\",\"true\",\"100\",\"01-02-1901 00:00:00\",\"1.2345\",\"\",\"0\",\"false\",\"\",\"0\",\"0\",\"0.0\",\"0.0\",\"\")", storeKey)
      //      val expected = "mockConnectionTracked(" + FIELDS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"),sent(\"\"),delivered(\"\"),viewed(\"\")))"
      val expected = "data(mockConnectionTracked("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00 00 00\"),amt(\"1 2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0 0\"),test8(\"0 0\"),test9(\"\"),test10(\"\"),sent(\"\"),delivered(\"\"),viewed(\"\"))))"
      storeKey must be_==(expected)
    }

    "generate store key correctly for Data with a Message and a Hashmap" in {
      val fromDetails = new java.util.HashMap[ String, Data ]()
      val tempProfile = new Profile()
      tempProfile.setFirstName("first")
      tempProfile.setLastName("last")
      fromDetails.put(tempProfile.formattedClassName, tempProfile)

      val postToTarget = new Post("subject2Target", "body", fromDetails)
      val postToBroker = new Post("subject2Broker", "body", fromDetails)
      val msg = new CreateInvitationRequest(null, "", "", "", "", "", "", "", postToTarget, postToBroker, false)
      val persistedMessage = new PersistedMessage(msg)
      persistedMessage.id = id

      val storeKey = persistedMessage.toStoreKey
      println("storeKey: " + storeKey)
      //      persistedMessage(fields(id("ec149c51-88f8-476c-bda7-8dd721d14a3f"),localeCode("en"),message(""),persisted("2012-05-02T22:01:02.903-05:00")))
      val persistedRaw = "" + persistedMessage.persisted
      val messageid = msg.ids.id
      val parentId = msg.ids.parentId
      val conversationId = msg.ids.conversationId
      val persisted = persistedRaw.replace(".", " ").replace(":"," ")
      val expected = "data(persistedMessage("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(message(\"\"),messageType(\"CreateInvitationRequest\"),messageId(\"" + messageid + "\"),messageParentId(\"" + parentId + "\"),messageConversationId(\"" + conversationId + "\"),persisted(\"" + persisted + "\"),viewed(\"\"),rejected(\"\"),ignored(\"\"),archived(\"\"))))"
      println("EXPECTED: " + expected)
      storeKey must be_==(expected)
    }

    "generate store key correctly for a Post" in {
        val fromDetails = new java.util.HashMap[ String, Data ]()
        val data: Post =  new Post("subject2Target", "body", fromDetails)
        val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
        data.id = id
        data.threadId = id
        val given = data.toStoreKey
        val expected = "data(post("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(subject(\"subject2Target\"),threadId(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),ignored(\"\"),archived(\"\"),sent(\"\"),delivered(\"\"),viewed(\"\"))))"
        given must be_==(expected)
      }

    "generate store key correctly for an Image" in {
      val data: Image = new Image("name", "type", "content hasn't be visible in a toStoreKey", "myMetadata")
      val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
      data.id = id
      val given = data.toStoreKey
      val expected = "data(image("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"name\"),contentType(\"type\"),metadata(\"myMetadata\"))))"
      //      println("given:" + given)
      //      println("expected:" + expected)
      given must be_==(expected)
    }

    "generate delete key correctly for a Profile" in {
      val idN = "MY_ID"
      val mockProfile = new Profile("FirstName", "LastName", "desc?", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      mockProfile.imageHashCode = "1234"
      mockProfile.id = idN
      mockProfile.localeCode = "en"
      val deleteKey = mockProfile.toDeleteKey
      //      val expectedStoreKey = "profile(fields(id(\"" + idN + "\"),localeCode(\"en\"),firstName(\"FirstName\"),lastName(\"LastName\"),description(\"\"),emailAddress(\"123456789@test.com\"),country(\"CA\"),region(\"someCAprovince\"),city(\"city\"),postalCode(\"postalCode\"),website(\"website\"),imageHashCode(_)))"
      val expectedStoreKey = "data(profile("  + KEYS + "(id(\"MY_ID\"),localeCode(_),recVerNum(_)),_))"
      //      println("created  storeKey4Profile: " + storeKey)
      //      println("expected storeKey4Profile: " + expectedStoreKey)
      deleteKey must be_==(expectedStoreKey)
    }

    "generate store key correctly for CompositeData" in {
        val connection = new Connection()
        connection.id = id

        val profile = new Profile()
        profile.firstName = "test"

        val compositeData = new CompositeData(connection, profile)

        val storeKey = compositeData.toStoreKey
        println("storeKey: " + storeKey)
        val expected = "compositeData(" + FIELDS + "(" + connection.toStoreKey + "," + profile.toStoreKey + "))"
        println("expected: " + expected)
        storeKey must be_==(expected)
      }
  }
}
