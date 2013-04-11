package com.protegra_ati.agentservices.core.schema.persistence

import org.specs2.mutable._
import java.util.UUID
import java.util.Locale
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core.messages.invitation.CreateInvitationRequest
import com.protegra_ati.agentservices.core.schema.SystemData
import com.protegra_ati.agentservices.core.schema.PersistedMessage

class StorableDataTest extends SpecificationWithJUnit
{
  val id = UUID.fromString("99595a09-8f3b-48a9-ad6d-ccd5d2782e71").toString
  "store key" should {
    "generate store key correctly for Data" in {
      val conn = new MockConnection
      conn.id = id
      val storeKey = conn.toStoreKey
      storeKey must be_==("data(mockConnection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"))))")
    }

    //Connection needs to be updated to output the right key again
    "generate store key correctly for Connection" in {

      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val connection = ConnectionFactory.createConnection("alias", ConnectionCategory.Business.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      connection.id = id

      val storeKey = connection.toStoreKey
      val expected = ( "data(connection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(category(\"Self\"),connectionType(\"connectionType\"),alias(\"alias\"),readCnxn(data(agentCnxnProxy(keys(id(\"\"),localeCode(\"\"),recVerNum(\"\")),fields(src(\"agent:  Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"),label(\"\"),trgt(\"agent:  Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"))))),writeCnxn(data(agentCnxnProxy(keys(id(\"\"),localeCode(\"\"),recVerNum(\"\")),fields(src(\"agent:  Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"),label(\"\"),trgt(\"agent:  Brokerbbbbbbbb-d417-4d71-ad94-8c766907381b \"))))),autoApprove(\"true\"),policies(\"[]\"),created(\"" + PrologFormatter.clean(connection.created.toString()) + "\"))))" )
      storeKey must be_==(expected)
    }

    "generate store key correctly for french Data" in {
      val conn = new MockConnection
      conn.id = id
      conn.setLocaleCode(Locale.FRENCH.toString())
      val storeKey = conn.toStoreKey
      storeKey must be_==("data(mockConnection(" + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"fr\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"))))")
    }

    "generate store key correctly for a System Data" in {
      val conn = new MockConnection
      conn.id = id
      val systemData = new SystemData[ MockConnection ](conn)
      val storeKey = systemData.toStoreKey
      storeKey must be_==("systemData(data(mockConnection("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\")))))")
    }

    "generate store key correctly for a Profile" in {
      val idN = "MY_ID"
      val mockProfile = new Profile("FirstName", "LastName?", "desc", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      mockProfile.imageHashCode = "1234"
      mockProfile.id = idN
      mockProfile.localeCode = "en"
      val storeKey = mockProfile.toStoreKey
      val expectedStoreKey = "data(profile(" + KEYS + "(id(\"MY_ID\"),localeCode(\"en\"),recVerNum(\"1\")),fields(firstName(\"FirstName\"),lastName(\"LastName \"),emailAddress(\"123456789@test.com\"),country(\"CA\"),region(\"someCAprovince\"),city(\"city\"),postalCode(\"postalCode\"),website(\"website\"),imageHashCode(\"1234\"))))"
      storeKey must be_==(expectedStoreKey)
    }


    "generate store key correctly for Data with Tracking" in {
      val trackedConn = new MockConnectionTracked
      trackedConn.id = id

      val storeKey = trackedConn.toStoreKey
      val expected = "data(mockConnectionTracked("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(name(\"Jennifer\"),isBlue(\"true\"),age(\"100\"),birthDate(\"01-02-1901 00:00:00\"),amt(\"1.2345\"),test0(\"[a, b]\"),test0Empty(\"[]\"),test1(\"\"),test2(\"0\"),test3(\"false\"),test4(\"\"),test5(\"0\"),test6(\"0\"),test7(\"0.0\"),test8(\"0.0\"),test9(\"\"),test10(\"\"),sent(\"\"),delivered(\"\"),viewed(\"\"))))"
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
      val persistedRaw = "" + persistedMessage.persisted
      val messageid = msg.ids.id
      val parentId = msg.ids.parentId
      val conversationId = msg.ids.conversationId
      val expected = "data(persistedMessage("  + KEYS + "(id(\"99595a09-8f3b-48a9-ad6d-ccd5d2782e71\"),localeCode(\"en\"),recVerNum(\"1\"))," + FIELDS + "(message(\"\"),messageType(\"CreateInvitationRequest\"),messageId(\"" + messageid + "\"),messageParentId(\"" + parentId + "\"),messageConversationId(\"" + conversationId + "\"),persisted(\"" + persistedRaw + "\"),viewed(\"\"),rejected(\"\"),ignored(\"\"),archived(\"\"))))"
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
      given must be_==(expected)
    }

    "generate delete key correctly for a Profile" in {
      val idN = "MY_ID"
      val mockProfile = new Profile("FirstName", "LastName", "desc?", "123456789@test.com", "CA", "someCAprovince", "city", "postalCode", "website")
      mockProfile.imageHashCode = "1234"
      mockProfile.id = idN
      mockProfile.localeCode = "en"
      val deleteKey = mockProfile.toDeleteKey
      val expectedStoreKey = "data(profile("  + KEYS + "(id(\"MY_ID\"),localeCode(_),recVerNum(_)),_))"
      deleteKey must be_==(expectedStoreKey)
    }

    "generate store key correctly for CompositeData" in {
        val connection = new Connection()
        connection.id = id

        val profile = new Profile()
        profile.firstName = "test"

        val compositeData = new CompositeData(connection, profile)

        val storeKey = compositeData.toStoreKey
        val expected = "compositeData(" + FIELDS + "(" + connection.toStoreKey + "," + profile.toStoreKey + "))"
        storeKey must be_==(expected)
      }
  }
}
