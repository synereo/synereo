package com.protegra_ati.agentservices.core.schema.util

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.util.UUID
import com.protegra_ati.agentservices.core._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import disclosure.{AppIdDisclosedDataFactory, TrustLevel, ProfileDisclosedDataFactory}
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
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.util.serializer.{Serializer, KryoSerializer}
import com.protegra_ati.agentservices.store.util.{Reporting, Severity}

class KryoSerializerTest extends SpecificationWithJUnit with Timeouts
{
  println(" in KryoSerializerTestDataInitializer constructor")


  //LIST OF IN KRYO NOT SUPPORTED SCALA FEATURES
  // keyword: "override val" in a constructor except for simple type like string !!!!
  // keyword: lazy val
  //

  "serializing mock data" should {
    // skipped("") // doesn't work for mocks can be deleted

    "serialize and deserialize mock object with a nested collection" in {


      KryoSerializerTestDataInitializer.serialize(MockWithJavaMap.simpleMockHashMapStringString, new File("serializedMockWithCollection.kryobin"))
      val deserializedMockMapStringString = KryoSerializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("serializedMockWithCollection.kryobin"))

      deserializedMockMapStringString must_!= null

      deserializedMockMapStringString match {
        case x: MockWithJavaMap => {
          x must be_==(MockWithJavaMap.simpleMockHashMapStringString)
          val keyValueRehydrated = x.map.get("KEY")
          val keyValue = MockWithJavaMap.simpleMockHashMapStringString.map.get("KEY")

          val key1ValueRehydrated = x.map1.get("KEY1")
          val key1Value = MockWithJavaMap.simpleMockHashMapStringString.map1.get("KEY1")

          keyValueRehydrated must be_==(keyValue)
          key1ValueRehydrated must be_==(key1Value)
        }
        case _ => failure
      }

      KryoSerializerTestDataInitializer.serialize(MockWithJavaMap.simpleMockHashMapStringData, new File("serializedMockWithCollection.kryobin"))
      val deserializedMockMapStringData = KryoSerializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("serializedMockWithCollection.kryobin"))

      deserializedMockMapStringData must_!= null

      deserializedMockMapStringData match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ Data ].id must be_==(MockWithJavaMap.simpleMockHashMapStringData.map.get("KEY").asInstanceOf[ Data ].id)
          x.map.get("KEY").asInstanceOf[ Data ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringData.map.get("KEY").asInstanceOf[ Data ].localeCode)
          x.map1.get("KEY1").asInstanceOf[ Data ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringData.map1.get("KEY1").asInstanceOf[ Data ].localeCode)

        }
        case _ => failure
      }


      KryoSerializerTestDataInitializer.serialize(MockWithJavaMap.simpleMockHashMapStringMockProfile, new File("serializedMockWithCollection.kryobin"))
      val deserializedMockMapStringMockProfile = KryoSerializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("serializedMockWithCollection.kryobin"))

      deserializedMockMapStringMockProfile must_!= null

      deserializedMockMapStringMockProfile match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].id must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].id)
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].localeCode)
          x.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].country must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map.get("KEY").asInstanceOf[ SimpleMockProfile1 ].country)
          x.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].country must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].country)
          x.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringMockProfile.map1.get("KEY1").asInstanceOf[ SimpleMockProfile1 ].localeCode)

        }
        case _ => failure
      }


      KryoSerializerTestDataInitializer.serialize(MockWithJavaMap.simpleMockHashMapStringProfile, new File("serializedMockWithCollection.kryobin"))
      val deserializedMockMapStringProfile = KryoSerializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("serializedMockWithCollection.kryobin"))

      deserializedMockMapStringProfile must_!= null

      deserializedMockMapStringProfile match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ Profile ].id must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].id)
          x.map.get("KEY").asInstanceOf[ Profile ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].localeCode)
          x.map.get("KEY").asInstanceOf[ Profile ].country must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].country)
        }
        case _ => failure
      }


      KryoSerializerTestDataInitializer.serialize(MockWithJavaMap.simpleMockHashMapStringProfileDownGradedToData, new File("serializedMockWithCollection.kryobin"))
      val deserializedMockMapStringProfileDownGraded = KryoSerializerTestDataInitializer.deserialize[ MockWithJavaMap ](new File("serializedMockWithCollection.kryobin"))

      deserializedMockMapStringProfileDownGraded must_!= null

      deserializedMockMapStringProfileDownGraded match {
        case x: MockWithJavaMap => {
          x.map.get("KEY").asInstanceOf[ Profile ].id must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].id)
          x.map.get("KEY").asInstanceOf[ Profile ].localeCode must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].localeCode)
          x.map.get("KEY").asInstanceOf[ Profile ].country must be_==(MockWithJavaMap.simpleMockHashMapStringProfile.map.get("KEY").asInstanceOf[ Profile ].country)
        }
        case _ => failure
      }
      success
    }


    "serialize and deserialize simple mock profile" in {
      // skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      KryoSerializerTestDataInitializer.serialize(mockSimpleProfile, new File("serializedMocProfile1.kryobin"))
      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_==(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
        }
        case _ => failure
      }
      success
    }


    "deserialize simple mock profile of a version with less parameter" in {
      skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedOlder//OneParameterLess_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_!=(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
          x.postalCode must be_==("") // missing, no value
        }
        case _ => failure
      }
      success
    }

    "deserialize simple mock profile of a version with less parameter in the middle" in {
      skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedOlder//OneParameterLessInTheMiddle_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_!=(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
          x.country must be_==("") // missing in the middle, no value
        }
        case _ => failure
      }
      success
    }


    "deserialize simple mock profile of a version with more parameter" in {
      skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedOlder//OneParameterMore_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_==(mockSimpleProfile) // no difference, parameter obove the present schema will be ignored
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
        }
        case _ => failure
      }
      success
    }


    "deserialize simple mock profile of a version with renamed parameter of same type" in {
      skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedOlder//RenamedParameterSameType_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_!=(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
          x.postalCode must be_==("") // renamed, no value
        }
        case _ => failure
      }
      success
    }

    "deserialize simple mock profile of a version with a parameter of changed type" in {
      skipped("ignore")
      val mockSimpleProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.profile1).asInstanceOf[ SimpleMockProfile1 ]
      mockSimpleProfile.id = "UID"
      mockSimpleProfile.localeCode = "br"

      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ SimpleMockProfile1 ](new File("serializedOlder//NotRenamedParameterDifferentType_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: SimpleMockProfile1 => {
          x must be_!=(mockSimpleProfile)
          x.id must be_==(mockSimpleProfile.id)
          x.firstName must be_==(mockSimpleProfile.firstName)
          x.classVersionNumber must be_==(mockSimpleProfile.classVersionNumber)
          x.city must be_==(mockSimpleProfile.city) // only in full schema
          x.postalCode must beNull[ String ] // renamed, no value, NULL POINTER are possible by changes of parameter type
        }
        case _ => failure
      }
      success
    }


    "serialize and deserialize composite mock profile" in {
      // skipped("ignore")
      val compositeMockProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.compositeProfile).asInstanceOf[ CompositeMockProfile1 ]
      compositeMockProfile.id = "UID"
      compositeMockProfile.localeCode = "br"
      compositeMockProfile.image.id = "nestedUID"
      compositeMockProfile.image.localeCode = "us"


      KryoSerializerTestDataInitializer.serialize(compositeMockProfile, new File("serializedCompositeMocProfile1.kryobin"))
      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ CompositeMockProfile1 ](new File("serializedCompositeMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: CompositeMockProfile1 => {
          x must be_==(compositeMockProfile)
          x.id must be_==(compositeMockProfile.id)
          x.firstName must be_==(compositeMockProfile.firstName)
          x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
          x.city must be_==(compositeMockProfile.city) // only in full schema
          x.image.name must be_==(compositeMockProfile.image.name) // same nested object
        }
        case _ => failure
      }
      success
    }


    "deserialize composite mock profile with one parameter more in the nested image object" in {
      skipped("ignore")
      val compositeMockProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.compositeProfile).asInstanceOf[ CompositeMockProfile1 ]
      compositeMockProfile.id = "UID"
      compositeMockProfile.localeCode = "br"
      compositeMockProfile.image.id = "nestedUID"
      compositeMockProfile.image.localeCode = "us"


      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ CompositeMockProfile1 ](new File("serializedOlder//compl//OneParameterMoreInsideNestedComposite_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: CompositeMockProfile1 => {
          x must be_==(compositeMockProfile)
          x.id must be_==(compositeMockProfile.id)
          x.firstName must be_==(compositeMockProfile.firstName)
          x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
          x.city must be_==(compositeMockProfile.city) // only in full schema
          x.image.name must be_==(compositeMockProfile.image.name) // same nested object
        }
        case _ => failure
      }
      success
    }

    "deserialize composite mock profile with one parameter more in the composite profile" in {
      skipped("ignore")
      val compositeMockProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.compositeProfile).asInstanceOf[ CompositeMockProfile1 ]
      compositeMockProfile.id = "UID"
      compositeMockProfile.localeCode = "br"
      compositeMockProfile.image.id = "nestedUID"
      compositeMockProfile.image.localeCode = "us"


      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ CompositeMockProfile1 ](new File("serializedOlder//compl//OneSimpleParameterMore_serializedCompositeMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: CompositeMockProfile1 => {
          x must be_==(compositeMockProfile)
          x.id must be_==(compositeMockProfile.id)
          x.firstName must be_==(compositeMockProfile.firstName)
          x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
          x.city must be_==(compositeMockProfile.city) // only in full schema
          x.image.name must be_==(compositeMockProfile.image.name) // same nested object
        }
        case _ => failure
      }
      success
    }

    "deserialize composite mock profile with one parameter less in the nested image object" in {

      skipped("ignore")
      val compositeMockProfile = new Cloner().deepClone(KryoSerializerTestDataInitializer.compositeProfile).asInstanceOf[ CompositeMockProfile1 ]
      compositeMockProfile.id = "UID"
      compositeMockProfile.localeCode = "br"
      compositeMockProfile.image.id = "nestedUID"
      compositeMockProfile.image.localeCode = "us"


      val deserializedMockProfile = KryoSerializerTestDataInitializer.deserialize[ CompositeMockProfile1 ](new File("serializedOlder//compl//OneParameterLessInsideNestedComposite_serializedMocProfile1.kryobin"))

      deserializedMockProfile must_!= null

      deserializedMockProfile match {
        case x: CompositeMockProfile1 => {
          x must be_!=(compositeMockProfile)
          x.id must be_==(compositeMockProfile.id)
          x.firstName must be_==(compositeMockProfile.firstName)
          x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
          x.city must be_==(compositeMockProfile.city) // only in full schema
          x.image.name must be_==(compositeMockProfile.image.name) // same nested object
          x.image.contentType must beNull[ String ] // missing, no value
        }
        case _ => failure
      }
      success
    }
  }

  "serializing real data" should {

    "serialize and deserialize composite with EventKey" in {
      // skipped("")
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val agentSessionId1: UUID = UUID.fromString("AE3C8348-94C2-4BF1-966A-EE57C14BA073")

      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val eventKey1 = new EventKey(agentSessionId1, "eventTag")

      val moc = new RealCompositeMockObject1(eventKey, eventKey1, "hi", "bai", new DateTime())

      KryoSerializerTestDataInitializer.serialize(moc, new File("serializedCompositeWithEventKey.kryobin"))

      val deserialized = KryoSerializerTestDataInitializer.deserialize[ RealCompositeMockObject1 ](new File("serializedCompositeWithEventKey.kryobin"))

      deserialized must_!= null
      deserialized match {
        case x: RealCompositeMockObject1 => {
          x.selfAlias must be_==(moc.selfAlias)

          x.eventKey must be_==(moc.eventKey)
          x.eventKey1 must be_==(moc.eventKey1)
          x.created must be_==(moc.created)
          x must be_==(moc)


        }
        case _ => failure
      }
      success

    }

    "serialize and deserialize different mock objects" in {
      // skipped("")
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val toDetails = new java.util.HashMap[ String, Data ]();
      val tempProfile = new Profile("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", "website");



      fromDetails.put(tempProfile.formattedClassName, tempProfile);
      toDetails.put(tempProfile.formattedClassName, tempProfile);

      val mockPostToBroker = new MockPost("subject: to Broker", "body", anyRefMap);

      val postToTarget = new Post("subject: to Target ", "body", fromDetails);
      val postToBroker = new Post("subject: to Broker", "body", fromDetails);


      val postToSerialized = new Post("subject: to Broker", "body", null, toDetails);

      // ------------------------------- Post

      KryoSerializerTestDataInitializer.serialize(postToBroker, new File("serializedMockPost.kryobin"))
      val deserializedMockPost = KryoSerializerTestDataInitializer.deserialize[ Post ](new File("serializedMockPost.kryobin"))

      deserializedMockPost must_!= null
      deserializedMockPost match {
        case x: Post => {
          x must be_==(postToBroker)
          x.id.toString must be_==(postToBroker.id)
          x.body must be_==(postToBroker.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToBroker.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
        }
        case _ => failure
      }

      //----------------------- Post
      KryoSerializerTestDataInitializer.serialize(postToSerialized, new File("serializedPost.kryobin"))
      val deserializedPost = KryoSerializerTestDataInitializer.deserialize[ Post ](new File("serializedPost.kryobin"))

      deserializedPost must_!= null
      deserializedPost match {
        case x: Post => {
          x must be_==(postToSerialized)
          x.id.toString must be_==(postToSerialized.id)
          x.body must be_==(postToSerialized.body)
          x.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName must be_==(postToSerialized.fromDetails.get(tempProfile.formattedClassName).asInstanceOf[ Profile ].lastName)
        }
        case _ => failure
      }

      // ----------------------------- Identification

      val id: Identification = new Identification("ID", "parent", "convers")
      KryoSerializerTestDataInitializer.serialize(id, new File("serializedIdentification.kryobin"))
      val deserializedIdentification = KryoSerializerTestDataInitializer.deserialize[ Identification ](new File("serializedIdentification.kryobin"))
      deserializedIdentification must_!= null
      deserializedIdentification match {
        case x: Identification => {
          x must be_==(id)
          x.conversationId must be_==(id.conversationId)
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
      val post = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap())

      val eventKeyHope = new MockEventKey(agentSessionId1, "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU")
      val mockRequest = new MockCreateInvitationRequest(eventKey, eventKeyHope, "invitationConnectionId1", "selfAlias", Some("hallo world"), None, post :: Nil)
      //mockRequest.originCnxn = newConn.writeCnxn

      KryoSerializerTestDataInitializer.serialize(mockRequest, new File("serializedMockRequest.kryobin"))
      val deserializedMockRequest = KryoSerializerTestDataInitializer.deserialize[ MockCreateInvitationRequest ](new File("serializedMockRequest.kryobin"))
      deserializedMockRequest must_!= null
      deserializedMockRequest match {
        case x: MockCreateInvitationRequest => {
          x must be_==(mockRequest)
          println("-->" + x)
          println("|-->" + mockRequest)



          x.eventKey must be_==(mockRequest.eventKey)
          x.eventKey2 must be_==(mockRequest.eventKey2)
          x.selfAlias must be_==(mockRequest.selfAlias)
          x.str must be_==(mockRequest.str)
          x.invitationConnectionId1 must be_==(mockRequest.invitationConnectionId1)
          //          x.someTest match {
          //            case None => failure("can't deserialize, none instead of string value")
          //            case Some(role) => {
          //              role.toString must be_==("hallo world")
          //            }
          //            case _ => println("else:" + x.someTest.getClass) //fail
          //          }
        }
      }

      val mockOption = new MockOptionObject()
      KryoSerializerTestDataInitializer.serialize(mockOption, new File("serializedMockOption.kryobin"))
      val deserializedMockOption = KryoSerializerTestDataInitializer.deserialize[ MockOptionObject ](new File("serializedMockOption.kryobin"))
      deserializedMockOption must_!= null
      deserializedMockOption match {
        case x: MockOptionObject => {
          x must be_==(mockOption)

        }
      }



      val agentCnxn = new MockAgentCnxnProxy("targetId".toURI, "I'm also here", "sourceId".toURI)

      KryoSerializerTestDataInitializer.serialize(agentCnxn, new File("agentCnxn.kryobin"))
      val deserializedAgentCnxnProxy = KryoSerializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("agentCnxn.kryobin"))
      deserializedAgentCnxnProxy must_!= null
      deserializedAgentCnxnProxy match {
        case x: MockAgentCnxnProxy => {
          x must be_==(agentCnxn)

        }
        case _ => failure
      }


      val agentCnxn1 = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)

      KryoSerializerTestDataInitializer.serialize(agentCnxn1, new File("agentCnxn.kryobin"))
      val deserializedAgentCnxnProxy1 = KryoSerializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("agentCnxn.kryobin"))
      deserializedAgentCnxnProxy1 must_!= null
      deserializedAgentCnxnProxy1 match {
        case x: MockAgentCnxnProxy => {
          x must be_==(agentCnxn1)
          // x.conversationId must be_==(id.conversationId)
        }
        case _ => failure
      }


      val agentCnxn2 = new MockAgentCnxnProxy("targetId".toURI, "Hi", null)

      KryoSerializerTestDataInitializer.serialize(agentCnxn2, new File("agentCnxn.kryobin"))
      val deserializedAgentCnxnProxy2 = KryoSerializerTestDataInitializer.deserialize[ MockAgentCnxnProxy ](new File("agentCnxn.kryobin"))
      deserializedAgentCnxnProxy2 must_!= null
      deserializedAgentCnxnProxy2 match {
        case x: MockAgentCnxnProxy => {
          x must be_==(agentCnxn2)
          // x.conversationId must be_==(id.conversationId)
        }
        case _ => failure
      }


      val category = "bla bla" //ConnectionCategory.None.toString
      val conn = MockConnFactory.createConnection("New Connection", category, "Custom", "self", "target", "false", List[ String ]("POL 1", "POL 2"));

      KryoSerializerTestDataInitializer.serialize(conn, new File("serializedConnection.kryobin"))
      val deserializedConnection = KryoSerializerTestDataInitializer.deserialize[ MockConn ](new File("serializedConnection.kryobin"))
      deserializedConnection must_!= null
      deserializedConnection match {
        case x: MockConn => {

          x mustEqual ( conn )
        }
        case _ => failure
      }




      val selfCnxn = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxn1 = new MockAgentCnxnProxy("targetId1".toURI, "", "sourceId1".toURI)



      val selfCnxns = new MockConn(ConnectionCategory.Self.toString, "Trusted", "System", selfCnxn, selfCnxn, "false", List[ String ]("DeleteDisabled", "SearchDisabled")) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))

      val systemConnection = new MockSystemData[ MockConn ](selfCnxns)

      KryoSerializerTestDataInitializer.serialize(systemConnection, new File("serializedSystemConnection.kryobin"))
      val deserializedSystemConnection = KryoSerializerTestDataInitializer.deserialize[ MockSystemData[ MockConn ] ](new File("serializedSystemConnection.kryobin"))
      deserializedSystemConnection must_!= null
      deserializedSystemConnection match {
        case x: MockSystemData[ MockConn ] => {

          x mustEqual ( systemConnection )
        }
        case _ => failure
      }
      success
    }

    "serialize and deserialize with same reference inside of " in {
      //skipped("")
      // up to 2 level references to the same object work, deeper levels require kryo.setReferences( false ) !!!
      val reference = new MockAgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)

      val duplicateHolder = new DuplicateMockObject(reference, reference, "test")

      KryoSerializerTestDataInitializer.serialize(duplicateHolder, new File("test.kryobin"))
      val deserializedDuplicateHolder = KryoSerializerTestDataInitializer.deserialize[ DuplicateMockObject ](new File("test.kryobin"))
      deserializedDuplicateHolder must_!= null
      deserializedDuplicateHolder match {
        case x: DuplicateMockObject => {

          x mustEqual ( duplicateHolder )
        }
        case _ => failure
      }

      success

    }

    "serialize and deserialize SystemData with Connection" in {
      // skipped("")
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)

      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Trusted", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))

      val systemConnection = new SystemData[ Connection ](selfCnxns)

      KryoSerializerTestDataInitializer.serialize(systemConnection, new File("serializedSystemConnection.kryobin"))
      val deserializedSystemConnection = KryoSerializerTestDataInitializer.deserialize[ SystemData[ Connection ] ](new File("serializedSystemConnection.kryobin"))
      deserializedSystemConnection must_!= null
      deserializedSystemConnection match {
        case x: SystemData[ Connection ] => {

          x mustEqual ( systemConnection )
        }
        case _ => failure
      }
      success

    }
    "serialize and deserialize connection using serializer class" in {
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)
      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Trusted", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))

      KryoSerializerTestDataInitializer.serialize(selfCnxns, new File("serializedConnection.kryobin"))
      val deserializedConnection = KryoSerializerTestDataInitializer.deserialize[ Connection ](new File("serializedConnection.kryobin"))
      deserializedConnection must_!= null
      deserializedConnection match {
        case x: Connection => {

          x mustEqual ( selfCnxns )
        }
        case _ => failure
      }


      val serializedConn = Serializer.serialize[ Connection ](selfCnxns)
      val deserializedConn = Serializer.deserialize[ Connection ](serializedConn)

      deserializedConn match {
        case x: Connection => {

          x mustEqual ( selfCnxns )
        }
        case _ => failure
      }
      success
    }





    "serialize and deserialize persisted CreateInvitationRequest" in {
      //skipped("")
      val agentSessionId: UUID = UUID.fromString("DE3C8348-94C2-4BF1-966A-EE57C14BA073")
      val eventTag = "content"
      val eventKey = new EventKey(agentSessionId, eventTag)
      val anyRefMap = new java.util.HashMap[ String, AnyRef ]();
      val fromDetails = new java.util.HashMap[ String, Data ]();
      val brokerId = "Broker" + UUID.fromString("bbbbbbbb-d417-4d71-ad94-8c766907381b")
      val postToTarget = new Post("subject: to Target ", "body", fromDetails);
      val postToBroker = new Post("subject: to Broker", "body", fromDetails);
      val newConn = ConnectionFactory.createConnection("New Connection", ConnectionCategory.App.toString, ConnectionCategory.Self.toString, "connectionType", brokerId, brokerId)

      newConn.id = "conn id"

      val msg = new CreateInvitationRequest(eventKey, "targetConnectionId", "selfAlias", "targetAlias", "selfCategory", "requestedCategory", "requestedConnectionType", "requestedConnectionName", null, null, false) //, postToTarget, postToBroker);
      //  msg.targetCnxn = newConn.readCnxn

      //  msg.ids.conversationId = "conversationId"
      //  msg.ids.parentId = "parentId"

      KryoSerializerTestDataInitializer.serialize(msg, new File("serializedRequest.kryobin"))
      val deserializedRequest = KryoSerializerTestDataInitializer.deserialize[ CreateInvitationRequest ](new File("serializedRequest.kryobin"))

      deserializedRequest must_!= null
      deserializedRequest match {
        case x: CreateInvitationRequest => {
          x must be_==(msg)
          x.brokerTargetCnxnKey.toString must be_==(msg.brokerTargetCnxnKey)
          x.eventKey must be_==(msg.eventKey)
          x.postToBroker must beNull[ Post ]
        }
        case _ => failure
      }


      val persistedMessage = new PersistedMessage[ CreateInvitationRequest ](msg)

      KryoSerializerTestDataInitializer.serialize(persistedMessage, new File("serializedPersistedMessageCreateInvitationRequest.kryobin"))
      val deserializedMessage = KryoSerializerTestDataInitializer.deserialize[ PersistedMessage[ CreateInvitationRequest ] ](new File("serializedPersistedMessageCreateInvitationRequest.kryobin"))
      deserializedMessage must_!= null
      deserializedMessage match {
        case x: PersistedMessage[ CreateInvitationRequest ] => {
          x.id must be_==(deserializedMessage.id)
          x.message must be_==(deserializedMessage.message)
          x must be_==(deserializedMessage)
        }
        case _ => failure
      }


      // KryoSerializerTestDataInitializer.serializeMockData(persistedMessage, new File("serializedPersistedMessageCreateInvitationRequest.kryobin"))

      // val deserialized = KryoSerializerTestDataInitializer.deserialize[ PersistedMessage[ CreateInvitationRequest ] ](new File("serializedPersistedMessageCreateInvitationRequest.kryobin"))

      //  deserialized must_!= null
      //      deserialized match {
      //        case x: PersistedMessage[ CreateInvitationRequest ] => {
      //          x must be_==(persistedMessage)
      //          //                x.id must be_==(compositeMockProfile.id)
      //          //                x.firstName must be_==(compositeMockProfile.firstName)
      //          //                x.classVersionNumber must be_==(compositeMockProfile.classVersionNumber)
      //          //                x.city must be_==(compositeMockProfile.city) // only in full schema
      //          //                x.image.name must be_==(compositeMockProfile.image.name) // same nested object
      //        }
      //        case _ => failure
      //      }
      success

    }
    "serialize and deserialize a DisclosedData[Profile] Object" in {
      val disclosedData = ProfileDisclosedDataFactory.getDisclosedData(TrustLevel.Trusted)
      KryoSerializerTestDataInitializer.serialize(disclosedData, new File("serializedDisclosedDataProfile.kryobin"))
      val deserializedMessage = KryoSerializerTestDataInitializer.deserialize[ DisclosedData[ Profile ] ](new File("serializedDisclosedDataProfile.kryobin"))
      deserializedMessage must_!= null
      deserializedMessage match {
        case x: DisclosedData[ Profile ] => {

          x must be_==(disclosedData)
        }
        case _ => failure
      }
      success
    }
    "serialize and deserialize a DisclosedData[AppId] Object" in {
      val disclosedData = AppIdDisclosedDataFactory.getDisclosedData(TrustLevel.Trusted)
      KryoSerializerTestDataInitializer.serialize(disclosedData, new File("serializedDisclosedDataAppId.kryobin"))
      val deserializedMessage = KryoSerializerTestDataInitializer.deserialize[ DisclosedData[ Profile ] ](new File("serializedDisclosedDataAppId.kryobin"))
      deserializedMessage must_!= null
      deserializedMessage match {
        case x: DisclosedData[ AppId ] => {

          x must be_==(disclosedData)
        }
        case _ => failure
      }
      success
    }

  }

}

object KryoSerializerTestDataInitializer extends Reporting
{
  val profile1: SimpleMockProfile1 = SimpleMockProfile1("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode")
  val compositeProfile: CompositeMockProfile1 = CompositeMockProfile1("firstName", "lastName", "description", "emailAddress", "country", "region", "city", "postalCode", MockImage.simpleDemoMockImage)

  def serialize(data: Object, toFile: File): Unit =
  {

    try {
      if ( toFile.exists() ) {toFile.delete()}
    } catch {
      case e: Throwable => report("Exception occured in serialize method", e, Severity.Error)
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
        case e: Throwable => report("Exception occured in serialize method", e, Severity.Error)
      }

    }
  }


  def deserialize[ T ](fromInputStream: InputStream): T =
  {
    KryoSerializer.getInstance().deserialize[ T ](fromInputStream)

  }

  def deserialize[ T ](file: File): T =
  {
    val inputStream = new FileInputStream(file)
    return deserialize[ T ](new BufferedInputStream(inputStream) )

  }

}