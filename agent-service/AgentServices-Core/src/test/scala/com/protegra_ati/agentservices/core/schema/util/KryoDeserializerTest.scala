package com.protegra_ati.agentservices.core.schema.util

import java.util.UUID
import com.protegra_ati.agentservices.core._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import messages._
import invitation.CreateInvitationRequest
import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._
import com.protegra_ati.agentservices.core.Timeouts
import scala.collection.JavaConversions._
import java.io._
import com.rits.cloning.Cloner
import com.esotericsoftware.kryo.Kryo
import java.nio.channels.FileChannel
import com.esotericsoftware.kryo.io._
import scala.None
import org.joda.time.DateTime
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.util.serializer.{Serializer, KryoSerializer}
import org.apache.commons.io.FileUtils

class KryoDeserializerTest extends SpecificationWithJUnit with Timeouts
{
//  println(" in KryoDeserializerTestDataInitializer constructor")


  "serializing mock data" should {

    "deserialize mock object with a nested collection" in {


      val deserializedMockMapStringString = KryoDeserializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("data//serializedMockWithCollection_StringStringMap.kryobin"))

      deserializedMockMapStringString must_!= null

      deserializedMockMapStringString match {
        case x: MockWithJavaMap => {
          x must be_==(MockWithJavaMap.simpleMockHashMapStringString)
          x.map.get("KEY").asInstanceOf[String] must be_==(MockWithJavaMap.simpleMockHashMapStringString.map.get("KEY").asInstanceOf[String])
          x.map1.get("KEY1").asInstanceOf[String] must be_==(MockWithJavaMap.simpleMockHashMapStringString.map1.get("KEY1").asInstanceOf[String])

          println("1 KRYO serialization succsessfull: " + x)

        }
        case _ => failure
      }



      val deserializedMockMapStringData = KryoDeserializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("data//serializedMockWithCollection_StringDataMap.kryobin"))

      deserializedMockMapStringData must_!= null

      deserializedMockMapStringData match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ Data ].id must be_==(MockWithJavaMap.simpleMockHashMapStringData.map.get("KEY").asInstanceOf[ Data ].id)
          x.map.get("KEY").asInstanceOf[ Data ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringData.map.get("KEY").asInstanceOf[ Data ].localeCode)
          x.map1.get("KEY1").asInstanceOf[ Data ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringData.map1.get("KEY1").asInstanceOf[ Data ].localeCode)
          println("2 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val deserializedMockMapStringMockProfile = KryoDeserializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("data//serializedMockWithCollection_StringMockProfileMap.kryobin"))

      deserializedMockMapStringMockProfile must_!= null

      deserializedMockMapStringMockProfile match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].id must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].id)
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].localeCode)
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].country must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].country)
          x.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].country must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].country)
          x.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].localeCode)
          println("3 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val deserializedMockMapStringProfile = KryoDeserializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("data//serializedMockWithCollection_StringProfileMap.kryobin"))

      deserializedMockMapStringProfile must_!= null

      deserializedMockMapStringProfile match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ Profile ].id must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].id)
          x.map.get("KEY").asInstanceOf[ Profile ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].localeCode)
          x.map.get("KEY").asInstanceOf[ Profile ].country must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].country)
          println("4 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }


    "deserialize simple mock profile" in {
      // skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoDeserializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoDeserializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("data//serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_==(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
          println("5 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }




    "deserialize composite mock profile" in {
      // skipped("ignore")
      val compositeMockProfile = new Cloner().deepClone(KryoDeserializerTestDataInitializer.compositeProfile).asInstanceOf[ CompositeMockProfile1 ]
      compositeMockProfile.id = "UID"
      compositeMockProfile.localeCode = "br"
      compositeMockProfile.image.id = "nestedUID"
      compositeMockProfile.image.localeCode = "us"


      val deserializedMockProfile = KryoDeserializerTestDataInitializer.deserialize[ CompositeMockProfile1 ](new File("data//serializedCompositeMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: CompositeMockProfile1 => {
          x must be_==(compositeMockProfile)
          x.id must be_==(compositeMockProfile.id)
          x.firstName must be_==(compositeMockProfile.firstName)
          x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
          x.city must be_==(compositeMockProfile.city) // only in full schema
          x.image.name must be_==(compositeMockProfile.image.name) // same nested object
          println("6 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }

  }

  "deserializing real data" should {

    "deserialize composite with EventKey" in {
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val agentSessionId1: UUID = UUID.fromString("AE3C8348-94C2-4BF1-966A-EE57C14BA073")

      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val eventKey1 = new EventKey(agentSessionId1, "eventTag")

      val moc = new RealCompositeMockObject1(eventKey, eventKey1, "hi", "bai", new DateTime(5000000))

      val deserialized = KryoDeserializerTestDataInitializer.deserialize[ RealCompositeMockObject1 ](new File("data//serializedCompositeWithEventKey.kryobin"))

      deserialized must_!= null
      deserialized match {
        case x: RealCompositeMockObject1 => {
          x.selfAlias must be_==(moc.selfAlias)

          x.eventKey must be_==(moc.eventKey)
          x.eventKey1 must be_==(moc.eventKey1)
          x.created must be_==(moc.created)
          x must be_==(moc)
          println("7 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success

    }

    "deserialize different mock objects" in {
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val toDetails = new java.util.HashMap[ String, Data ]();
      val tempProfile = new Profile("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", "website");
      fromDetails.put(tempProfile.formattedClassName, tempProfile);
      toDetails.put(tempProfile.formattedClassName, tempProfile);
      val postToBroker = new Post("subject: to Broker", "body", new java.util.HashMap(), fromDetails, "AE3C8348-94C2-4BF1-966A-EE57C14BA073");
      val postToSerialized = new Post("subject: to Broker", "body", null, toDetails, "AE3C8348-94C2-4BF1-966A-EE57C14BA073");
      val deserializedMockPost = KryoDeserializerTestDataInitializer.deserialize[ Post ](new File("data//serializedMockPost.kryobin"))
      deserializedMockPost must_!= null
      deserializedMockPost match {
        case x: Post => {
          x must be_==(postToBroker)
          x.id.toString must be_==(postToBroker.id)
          x.body must be_==(postToBroker.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToBroker.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
          println("8 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val deserializedPost = KryoDeserializerTestDataInitializer.deserialize[ Post ](new File("data//serializedPost.kryobin"))

      deserializedPost must_!= null
      deserializedPost match {
        case x: Post => {
          x must be_==(postToSerialized)
          x.id.toString must be_==(postToSerialized.id)
          x.body must be_==(postToSerialized.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToSerialized.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
          println("9 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val id: Identification = new Identification("ID", "parent", "convers")
      val deserializedIdentification = KryoDeserializerTestDataInitializer.deserialize[ Identification ](new File("data//serializedIdentification.kryobin"))
      deserializedIdentification must_!= null
      deserializedIdentification match {
        case x: Identification => {
          x must be_==(id)
          x.conversationId must be_==(id.conversationId)
          println("10 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val agentSessionId1: UUID = UUID.fromString("AE3C8348-94C2-4BF1-966A-EE57C14BA070")
      val eventTag = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      newConn.id = "conn id"

      // ------------------------------MockCreateInvitationRequest
      val eventKey = new MockEventKey(agentSessionId, eventTag)
      val post = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap(), "bbbbbbbb-d417-4d71-ad94-8c766907381b")

      val eventKeyHope = new MockEventKey(agentSessionId1, "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU")
      val mockRequest = new MockCreateInvitationRequest(eventKey, eventKeyHope, "invitationConnectionId1", "selfAlias", Some("hallo world"), None, post :: Nil)
      val deserializedMockRequest = KryoDeserializerTestDataInitializer.deserialize[ MockCreateInvitationRequest ](new File("data//serializedMockRequest.kryobin"))
      deserializedMockRequest must_!= null
      deserializedMockRequest match {
        case x: MockCreateInvitationRequest => {

          println("-->" + x)
          println("|-->" + mockRequest)
          x must be_==(mockRequest)
          x.eventKey must be_==(mockRequest.eventKey)
          x.eventKey2 must be_==(mockRequest.eventKey2)
          x.selfAlias must be_==(mockRequest.selfAlias)
          x.str must be_==(mockRequest.str)
          x.invitationConnectionId1 must be_==(mockRequest.invitationConnectionId1)
          println("11 KRYO serialization succsessfull: " + x)
        }
      }

      val mockOption = new MockOptionObject()
      val deserializedMockOption = KryoDeserializerTestDataInitializer.deserialize[ MockOptionObject ](new File("data//serializedMockOption.kryobin"))
      deserializedMockOption must_!= null
      deserializedMockOption match {
        case x: MockOptionObject => {
          x must be_==(mockOption)
          println("12 KRYO serialization succsessfull: " + x)
        }
      }



      val agentCnxn = new MockAgentCnxnProxy("targetId".toURI, "I'm also here", "sourceId".toURI)

      val deserializedAgentCnxnProxy = KryoDeserializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("data//agentCnxn1.kryobin"))
      deserializedAgentCnxnProxy must_!= null
      deserializedAgentCnxnProxy match {
        case x: MockAgentCnxnProxy => {
          x.src.toString must be_==(agentCnxn.src.toString)
          x.label must be_==(agentCnxn.label)
          println("13 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentCnxn1 = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val deserializedAgentCnxnProxy1 = KryoDeserializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("data//agentCnxn2.kryobin"))
      deserializedAgentCnxnProxy1 must_!= null
      deserializedAgentCnxnProxy1 match {
        case x: MockAgentCnxnProxy => {
          x.label must be_==(agentCnxn1.label)
          println("14 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentCnxn2 = new MockAgentCnxnProxy("targetId".toURI, "Hi", null)
      val deserializedAgentCnxnProxy2 = KryoDeserializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("data//agentCnxn3.kryobin"))
      deserializedAgentCnxnProxy2 must_!= null
      deserializedAgentCnxnProxy2 match {
        case x: MockAgentCnxnProxy => {
          x.src.toString must be_==(agentCnxn2.src.toString)
          x.trgt must be ( null )
          println("15 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success

    }

    "deserialize with same reference inside of " in {
      // up to 2 level references to the same object work, deeper levels require kryo.setReferences( false ) !!!
      val reference = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val duplicateHolder = new DuplicateMockObject(reference, reference, "test")
      val deserializedDuplicateHolder = KryoDeserializerTestDataInitializer.deserialize[ DuplicateMockObject ](new File("data//test.kryobin"))
      deserializedDuplicateHolder must_!= null
      deserializedDuplicateHolder match {
        case x: DuplicateMockObject => {
          x.str mustEqual ( duplicateHolder.str )
          println("16 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      success
    }

    "deserialize SystemData with Connection" in {
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))
      val systemConnection = new SystemData[ Connection ](selfCnxns)
      val deserializedSystemConnection = KryoDeserializerTestDataInitializer.deserialize[ SystemData[ Connection ] ](new File("data//serializedSystemConnection.kryobin"))
      deserializedSystemConnection must_!= null
      deserializedSystemConnection match {
        case x: SystemData[ Connection ] => {
          x mustEqual ( systemConnection )
          println("17 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }

    "serialize and deserialize connection using serializer class" in {
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))
      val deserializedConnection = KryoSerializerTestDataInitializer.deserialize[ Connection ](new File("data//serializedConnection.kryobin"))
      deserializedConnection must_!= null
      deserializedConnection match {
        case x: Connection => {
          x mustEqual ( selfCnxns )
          println("18 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val serializedConn = Serializer.serialize[ Connection ](selfCnxns)
      val deserializedConn = Serializer.deserialize[ Connection ](serializedConn)

      deserializedConn match {
        case x: Connection => {
          x mustEqual ( selfCnxns )
          println("19 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }





    "deserialize persisted CreateInvitationRequest" in {
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.App.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      newConn.id = "conn id"
      val msg = new CreateInvitationRequest(eventKey, "targetConnectionId", "selfAlias", "targetAlias", "selfCategory", "requestedCategory", "requestedConnectionType", "requestedConnectionName", null, null) //, postToTarget, postToBroker);
      val deserializedRequest = KryoDeserializerTestDataInitializer.deserialize[ CreateInvitationRequest ](new File("data//serializedRequest.kryobin"))

      deserializedRequest must_!= null
      deserializedRequest match {
        case x: CreateInvitationRequest => {
          x must be_==(msg)
          x.invitationConnectionId.toString must be_==(msg.invitationConnectionId)
          x.eventKey must be_==(msg.eventKey)
          x.postToBroker must beNull[ Post ]
          println("20 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val deserializedMessage = KryoDeserializerTestDataInitializer.deserialize[ PersistedMessage[ CreateInvitationRequest ] ](new File("data//serializedPersistedMessageCreateInvitationRequest.kryobin"))
      deserializedMessage must_!= null
      deserializedMessage match {
        case x: PersistedMessage[ CreateInvitationRequest ] => {
          x.id must be_==(deserializedMessage.id)
          x.message must be_==(deserializedMessage.message)
          x must be_==(deserializedMessage)
          println("21 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }
  }


  "deserializing from string real data" should {

    "deserialize composite with EventKey" in {
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val agentSessionId1: UUID = UUID.fromString("AE3C8348-94C2-4BF1-966A-EE57C14BA073")

      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val eventKey1 = new EventKey(agentSessionId1, "eventTag")

      val moc = new RealCompositeMockObject1(eventKey, eventKey1, "hi", "bai", new DateTime(5000000))

      val deserialized = KryoDeserializerTestDataInitializer.deserializeFromString[ RealCompositeMockObject1 ](new File("data//serializedCompositeWithEventKey.kryostring"))

      deserialized must_!= null
      deserialized match {
        case x: RealCompositeMockObject1 => {
          x.selfAlias must be_==(moc.selfAlias)

          x.eventKey must be_==(moc.eventKey)
          x.eventKey1 must be_==(moc.eventKey1)
          x.created must be_==(moc.created)
          x must be_==(moc)
          println("7 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }

    "deserialize different mock objects" in {
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val toDetails = new java.util.HashMap[ String, Data ]();
      val tempProfile = new Profile("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", "website");
      fromDetails.put(tempProfile.formattedClassName, tempProfile);
      toDetails.put(tempProfile.formattedClassName, tempProfile);
      val postToBroker = new Post("subject: to Broker", "body", new java.util.HashMap(), fromDetails, "AE3C8348-94C2-4BF1-966A-EE57C14BA073");
      val postToSerialized = new Post("subject: to Broker", "body", null, toDetails, "AE3C8348-94C2-4BF1-966A-EE57C14BA073");
      val deserializedMockPost = KryoDeserializerTestDataInitializer.deserializeFromString[ Post ](new File("data//serializedMockPost.kryostring"))
      deserializedMockPost must_!= null
      deserializedMockPost match {
        case x: Post => {
          x must be_==(postToBroker)
          x.id.toString must be_==(postToBroker.id)
          x.body must be_==(postToBroker.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToBroker.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
          println("8 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val deserializedPost = KryoDeserializerTestDataInitializer.deserializeFromString[ Post ](new File("data//serializedPost.kryostring"))

      deserializedPost must_!= null
      deserializedPost match {
        case x: Post => {
          x must be_==(postToSerialized)
          x.id.toString must be_==(postToSerialized.id)
          x.body must be_==(postToSerialized.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToSerialized.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
          println("9 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val id: Identification = new Identification("ID", "parent", "convers")
      val deserializedIdentification = KryoDeserializerTestDataInitializer.deserializeFromString[ Identification ](new File("data//serializedIdentification.kryostring"))
      deserializedIdentification must_!= null
      deserializedIdentification match {
        case x: Identification => {
          x must be_==(id)
          x.conversationId must be_==(id.conversationId)
          println("10 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val agentSessionId1: UUID = UUID.fromString("AE3C8348-94C2-4BF1-966A-EE57C14BA070")
      val eventTag = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      newConn.id = "conn id"

      // ------------------------------MockCreateInvitationRequest
      val eventKey = new MockEventKey(agentSessionId, eventTag)
      val post = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap(), "bbbbbbbb-d417-4d71-ad94-8c766907381b")

      val eventKeyHope = new MockEventKey(agentSessionId1, "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU")
      val mockRequest = new MockCreateInvitationRequest(eventKey, eventKeyHope, "invitationConnectionId1", "selfAlias", Some("hallo world"), None, post :: Nil)
      val deserializedMockRequest = KryoDeserializerTestDataInitializer.deserializeFromString[ MockCreateInvitationRequest ](new File("data//serializedMockRequest.kryostring"))
      deserializedMockRequest must_!= null
      deserializedMockRequest match {
        case x: MockCreateInvitationRequest => {

          println("-->" + x)
          println("|-->" + mockRequest)
          x must be_==(mockRequest)
          x.eventKey must be_==(mockRequest.eventKey)
          x.eventKey2 must be_==(mockRequest.eventKey2)
          x.selfAlias must be_==(mockRequest.selfAlias)
          x.str must be_==(mockRequest.str)
          x.invitationConnectionId1 must be_==(mockRequest.invitationConnectionId1)
          println("11 KRYO serialization succsessfull: " + x)
        }
      }

      val mockOption = new MockOptionObject()
      val deserializedMockOption = KryoDeserializerTestDataInitializer.deserializeFromString[ MockOptionObject ](new File("data//serializedMockOption.kryostring"))
      deserializedMockOption must_!= null
      deserializedMockOption match {
        case x: MockOptionObject => {
          x must be_==(mockOption)
          println("12 KRYO serialization succsessfull: " + x)
        }
      }



      val agentCnxn = new MockAgentCnxnProxy("targetId".toURI, "I'm also here", "sourceId".toURI)

      val deserializedAgentCnxnProxy = KryoDeserializerTestDataInitializer.deserializeFromString[ MockAgentCnxnProxy ](new File("data//agentCnxn1.kryostring"))
      deserializedAgentCnxnProxy must_!= null
      deserializedAgentCnxnProxy match {
        case x: MockAgentCnxnProxy => {
          x.src.toString must be_==(agentCnxn.src.toString)
          x.label must be_==(agentCnxn.label)
          println("13 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentCnxn1 = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val deserializedAgentCnxnProxy1 = KryoDeserializerTestDataInitializer.deserializeFromString[ MockAgentCnxnProxy ](new File("data//agentCnxn2.kryostring"))
      deserializedAgentCnxnProxy1 must_!= null
      deserializedAgentCnxnProxy1 match {
        case x: MockAgentCnxnProxy => {
          x.label must be_==(agentCnxn1.label)
          println("14 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }


      val agentCnxn2 = new MockAgentCnxnProxy("targetId".toURI, "Hi", null)
      val deserializedAgentCnxnProxy2 = KryoDeserializerTestDataInitializer.deserializeFromString[ MockAgentCnxnProxy ](new File("data//agentCnxn3.kryostring"))
      deserializedAgentCnxnProxy2 must_!= null
      deserializedAgentCnxnProxy2 match {
        case x: MockAgentCnxnProxy => {
          x.src.toString must be_==(agentCnxn2.src.toString)
          x.trgt must be ( null )
          println("15 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }

    "deserialize with same reference inside of " in {
      // up to 2 level references to the same object work, deeper levels require kryo.setReferences( false ) !!!
      val reference = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val duplicateHolder = new DuplicateMockObject(reference, reference, "test")
      val deserializedDuplicateHolder = KryoDeserializerTestDataInitializer.deserializeFromString[ DuplicateMockObject ](new File("data//test.kryostring"))
      deserializedDuplicateHolder must_!= null
      deserializedDuplicateHolder match {
        case x: DuplicateMockObject => {
          x.str mustEqual ( duplicateHolder.str )
          println("16 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      success
    }

    "deserialize SystemData with Connection" in {
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))
      val systemConnection = new SystemData[ Connection ](selfCnxns)
      val deserializedSystemConnection = KryoDeserializerTestDataInitializer.deserializeFromString[ SystemData[ Connection ] ](new File("data//serializedSystemConnection.kryostring"))
      deserializedSystemConnection must_!= null
      deserializedSystemConnection match {
        case x: SystemData[ Connection ] => {
          x mustEqual ( systemConnection )
          println("17 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }

    "serialize and deserialize connection using serializer class" in {
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))
      var i = 0
      while ( i < 5000 ) {
        val deserializedConnection = KryoDeserializerTestDataInitializer.deserializeFromString[ Connection ](new File("data//serializedConnection.kryostring"))
        deserializedConnection must_!= null
        deserializedConnection match {
          case x: Connection => {
            x mustEqual ( selfCnxns )
            println("18 KRYO serialization succsessfull: " + x)
          }
          case _ => failure
        }
        Thread.sleep(50)
      i += 1
      }

      val serializedConn = Serializer.serialize[ Connection ](selfCnxns)
      val deserializedConn = Serializer.deserialize[ Connection ](serializedConn)

      deserializedConn match {
        case x: Connection => {
          x mustEqual ( selfCnxns )
          println("19 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }





    "deserialize persisted CreateInvitationRequest" in {
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.App.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)
      newConn.id = "conn id"
      val msg = new CreateInvitationRequest(eventKey, "targetConnectionId", "selfAlias", "targetAlias", "selfCategory", "requestedCategory", "requestedConnectionType", "requestedConnectionName", null, null) //, postToTarget, postToBroker);
      val deserializedRequest = KryoDeserializerTestDataInitializer.deserializeFromString[ CreateInvitationRequest ](new File("data//serializedRequest.kryostring"))

      deserializedRequest must_!= null
      deserializedRequest match {
        case x: CreateInvitationRequest => {
          x must be_==(msg)
          x.invitationConnectionId.toString must be_==(msg.invitationConnectionId)
          x.eventKey must be_==(msg.eventKey)
          x.postToBroker must beNull[ Post ]
          println("20 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }

      val deserializedMessage = KryoDeserializerTestDataInitializer.deserializeFromString[ PersistedMessage[ CreateInvitationRequest ] ](new File("data//serializedPersistedMessageCreateInvitationRequest.kryostring"))
      deserializedMessage must_!= null
      deserializedMessage match {
        case x: PersistedMessage[ CreateInvitationRequest ] => {
          x.id must be_==(deserializedMessage.id)
          x.message must be_==(deserializedMessage.message)
          x must be_==(deserializedMessage)
          println("21 KRYO serialization succsessfull: " + x)
        }
        case _ => failure
      }
      success
    }
  }


}

object KryoDeserializerTestDataInitializer
{
  val profile1: SimpleMockProfile1 = SimpleMockProfile1("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode")
  val compositeProfile: CompositeMockProfile1 = CompositeMockProfile1("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", MockImage.simpleDemoMockImage)

  def serialize(data: Object, toFile: File): Unit =
  {
    try {
      if ( toFile.exists() ) {toFile.delete()}
    } catch {
      case ex: Exception => {
        /* TODO into log file*/
        println("something different:" + ex.toString)
        ex.printStackTrace()
//        failure("previously serialized class can't be deleted")
        println("previously serialized class can't be deleted")
      }
    }
    serialize(data, new FileOutputStream(toFile))
  }


  def serialize(data: Object, toStream: FileOutputStream): Unit =
  {
    val _serializer = KryoSerializer.getInstance()

    val out: BufferedOutputStream = new BufferedOutputStream(toStream)
    try {
      _serializer.serialize(data, out)

    } finally {
      try {
        if ( out != null ) {
          out.close();
        }
      } catch {
        case ex: IOException => {/* TODO into log file*/}
        case _ => {/* TODO into log file*/}
      }

    }
  }


  def deserialize[ T ](fromInputStream: InputStream): T =
  {
    KryoSerializer.getInstance().deserialize[ T ](fromInputStream)
  }

  def deserialize[ T ](file: File): T =
  {
    return deserialize[ T ](new BufferedInputStream(new FileInputStream(file)))
  }

  def deserializeFromString[ T ](from: File): T =
  {
    try {
      val base64 = FileUtils.readFileToString(from, "UTF-8")
      return Serializer.deserialize[ T ](base64)
    } catch {
      case ex: IOException => {
        /* TODO into log file*/
        println("ERROR BY DESERIALIZATION")
        ex.printStackTrace()
        return null.asInstanceOf[ T ]
      }
      case ex: Exception => {
        /* TODO into log file*/
        println("ERROR BY DESERIALIZATION")
        ex.printStackTrace()
        return null.asInstanceOf[ T ]
      }
      case _ => {
        println("ERROR BY DESERIALIZATION")
        /* TODO into log file*/
        return null.asInstanceOf[ T ]
      }
    }
  }

}