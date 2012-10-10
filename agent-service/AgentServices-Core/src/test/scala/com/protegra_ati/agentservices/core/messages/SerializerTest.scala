/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages

import invitation.{ReferralRequest, CreateInvitationRequest, InvitationRequest}
import java.util.UUID
import org.apache.ws.commons.util.Base64
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.PersistedMessage
import com.protegra_ati.agentservices.core.schema.util._
import disclosure.{TrustLevel}
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.File
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs._
import com.protegra_ati.agentservices.core._
import persistence.MockConnection
import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.util.serializer.{Serializer, KryoSerializer}
import org.joda.time.DateTime
class SerializerTest
  extends JUnit4(SerializerTestSpecs)

object SerializerTestSpecsRunner
  extends ConsoleRunner(SerializerTestSpecs)

object SerializerTestSpecs extends Specification with
Timeouts
{
  "serialize" should {
    val maxKB = 25 * 1024

    "deserialize Message " in {
      val parentIds = new Identification()
      val msg = new GetContentResponse(parentIds.copyAsChild(), null, null)

      val serializedMsg = Serializer.serialize[ Message ](msg)
      val deserializedMsg = Serializer.deserialize[ Message ](serializedMsg)

      deserializedMsg match {
        case x: GetContentResponse => {
          x must be_==(msg)
          x.ids must be_==(msg.ids)
          //            assertEquals(x.ids.id, msg.ids.id)
          //            assertEquals(x.ids.parentId, msg.ids.parentId)
        }
        case _ => fail
      }
    }
    "deserialize Request " in {
      val search = new Connection()
      val msg = new GetContentRequest(null, search)

      val serializedMsg = Serializer.serialize[ Message ](msg)
      val deserializedMsg = Serializer.deserialize[ Message ](serializedMsg)

      deserializedMsg match {
        case x: GetContentRequest => {
          x must be_==(msg)
          x.ids must be_==(msg.ids)
          //            assertEquals(x.ids.id, msg.ids.id)
          //            assertEquals(x.ids.parentId, msg.ids.parentId)
        }
        case _ => fail
      }
    }

    "deserialize simple Data " in {
      val data = new Profile("test", "data", "test Description", "1@test.com", "CA", "someCAprovince", "city", "postalCode", "website")

      val serializedData = Serializer.serialize[ Data ](data)
      val deserializedData = Serializer.deserialize[ Data ](serializedData)

      deserializedData match {
        case x: Profile => {
          x must be_==(data)
          x.id must be_==(data.id)
          x.firstName must be_==(data.firstName)
        }
        case _ => fail
      }
    }

    "serialize and deserialize SystemData with Connection" in {
      skip("Until Kryo back in place")
      val selfCnxn = new AgentCnxnProxy("targetId".toURI, "", "sourceId".toURI)

      val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ](ConnectionPolicy.DeleteDisabled.toString, ConnectionPolicy.SearchDisabled.toString)) //List[ String ]()) //List[ String ]("DeleteDisabled", "SearchDisabled"))

      val systemConnection = new SystemData[ Connection ](selfCnxns)

      KryoSerializerTestDataInitializer.serialize(systemConnection, new File("serializedSystemConnection.kryobin"))
      val deserializedSystemConnection = KryoSerializerTestDataInitializer.deserialize[ SystemData[ Connection ] ](new File("serializedSystemConnection.kryobin"))
      deserializedSystemConnection must_!= null
      deserializedSystemConnection match {
        case x: SystemData[ Connection ] => {

          x mustEqual ( systemConnection )
        }
        case _ => fail
      }

    }

    "deserialize mock Data " in {
      val data = new MockConnection()
      val serializedData = Serializer.serialize[ Data ](data)
      val deserializedData = Serializer.deserialize[ Data ](serializedData)

      deserializedData match {
        case x: MockConnection => {
          x.name must be_==(data.name)
        }
        case _ => fail
      }
    }

    "deserialize connection Data " in {
      val data = new Connection("connectionType", ConnectionCategory.Self.toString, "alias", null, null, "autoApprove", List(ConnectionPolicy.ReferralsDisabled.toString, ConnectionPolicy.SearchDisabled.toString))

      val serializedData = Serializer.serialize[ Data ](data)
      val deserializedData = Serializer.deserialize[ Data ](serializedData)

      deserializedData match {
        case x: Connection => {
          x.alias must be_==(data.alias)
          x.policies must be_==(data.policies)
        }
        case _ => fail
      }
    }

    "deserialize tracking Data " in {
      val data = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap())

      val serializedData = Serializer.serialize[ Data ](data)
      val deserializedData = Serializer.deserialize[ Data ](serializedData)

      deserializedData match {
        case x: Post => {
          x must be_==(data)
          x.id must be_==(data.id)
          x.subject must be_==(data.subject)
        }
        case _ => fail
      }
    }


    "deserialize InvitationRequest" in {
      val inviteRequest = new InvitationRequest(new Identification(), new EventKey(UUID.randomUUID(), "tydegfrtew"), "IntroID", None, Some("Basic"), Some("Test Connection"), new Post() :: Nil)

      val serialized = Serializer.serialize[ InvitationRequest ](inviteRequest)
      val deserializedData = Serializer.deserialize[ InvitationRequest ](serialized)

      deserializedData match {
        case x: InvitationRequest => {
          x must be_==(inviteRequest)
          x.getIds() must be_==(inviteRequest.getIds())
          x.getEventKey() must be_==(inviteRequest.getEventKey())
        }
        case _ => fail
      }

    }


    "deserialize some mock message" in {

      val message = new MockDirectMessage(new Identification(), new EventKey(UUID.randomUUID(), "tydegfrtew"), "works", Some("Hi"), None, Some("Basic"), new Post() :: Nil)

      val serialized = Serializer.serialize[ MockDirectMessage ](message)

      val deserializedData = Serializer.deserialize[ MockDirectMessage ](serialized)

      deserializedData match {
        case x: MockDirectMessage => {
          x must be_==(message)
          x.eventKey must be_==(message.eventKey)
          x.ids must be_==(message.ids)
        }
        case _ => fail
      }

    }


    "deserialize some persisted mock message" in {
      skip("")
      val message = new MockDirectMessage(new Identification(), new EventKey(UUID.randomUUID(), "tydegfrtew"), "works", Some("Hi"), None, Some("Basic"), new Post() :: Nil)
      val data = new PersistedMessage[ MockDirectMessage ](message)

      Serializer.evaluateSerializerClass(data) mustEqual ( KryoSerializer.getInstance().getClass.getName )
      val serialized = Serializer.serialize[ PersistedMessage[ MockDirectMessage ] ](data)
      val deserializedData = Serializer.deserialize[ PersistedMessage[ MockDirectMessage ] ](serialized)

      deserializedData match {
        case x: PersistedMessage[ MockDirectMessage ] => {
          //x must be_==(data)
          x.getArchived must beNull
          data.getArchived must beNull
          //x.getRejected must be_==(data.getRejected)
          x.getIgnored must beNull
          data.getIgnored must beNull

          System.err.println("x=" + x)
          System.err.println("data=" + data)
          x.getMessage() must be_==(data.getMessage())

        }
        case _ => fail
      }

    }


    "deserialize persisted InvitationRequest" in {
      // skip("")

      val inviteRequest = new InvitationRequest(new Identification(), new EventKey(UUID.randomUUID(), "tydegfrtew"),
        "IntroID", None, Some("Basic"), Some("Test Connection"), new Post() :: Nil)
      val data = new PersistedMessage[ InvitationRequest ](inviteRequest)
//      Serializer.evaluateSerializerClass(data) mustEqual ( KryoSerializer.getInstance().getClass ).getName

      val serialized = Serializer.serialize[ PersistedMessage[ InvitationRequest ] ](data)
      val deserializedData = Serializer.deserialize[ PersistedMessage[ InvitationRequest ] ](serialized)

      deserializedData match {
        case x: PersistedMessage[ InvitationRequest ] => {

          x.id must be_==(data.id)
          System.err.println("InvitationRequest before :" + inviteRequest)
          System.err.println("InvitationRequest after :" + x.message)
          x.getMessage() must be_==(data.getMessage())
          x must be_==(data)
        }
        case _ => fail
      }

    }



    "deserialize persisted ReferralRequest" in {
      // skip("")

      val sourceRequest = new CreateInvitationRequest(new EventKey(UUID.randomUUID(), "tydegfrtew"), "targetConnectionId", "selfAlias", "targetAlias", "requestedCategory", "requestedConnectionType", "requestedConnectionName", null, null) //, postToTarget, postToBroker);
      val req = new ReferralRequest(sourceRequest.ids.copyAsChild(), sourceRequest.eventKey, sourceRequest)
      val data = new PersistedMessage[ ReferralRequest ](req)
//      Serializer.evaluateSerializerClass(data) mustEqual ( KryoSerializer.getInstance().getClass.getName )
      val serialized = Serializer.serialize[ PersistedMessage[ ReferralRequest ] ](data)
      val deserializedData = Serializer.deserialize[ PersistedMessage[ ReferralRequest ] ](serialized)

      deserializedData match {
        case x: PersistedMessage[ ReferralRequest ] => {

          x.id must be_==(data.id)
          System.err.println("InvitationRequest before :" + req)
          System.err.println("InvitationRequest after :" + x.message)
          x.getMessage().toString must be_==(data.getMessage().toString)
          x must be_==(data)
        }
        case _ => fail
      }


    }

    "deserialize persisted CreateInvitationRequest" in {
      //  skip("")
      val post1 = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap())
      val post2 = new Post("theSubject", "theBody", new java.util.HashMap(), new java.util.HashMap())

      val sourceRequest = new CreateInvitationRequest(new EventKey(UUID.randomUUID(), "tydegfrtew"), "targetConnectionId", "selfAlias", "targetAlias", "requestedCategory", "requestedConnectionType", "requestedConnectionName", post1, post2) //, postToTarget, postToBroker);

//      Serializer.evaluateSerializerClass(sourceRequest) mustEqual ( KryoSerializer.getInstance().getClass.getName )
      val serializedRequest = Serializer.serialize[ CreateInvitationRequest ](sourceRequest)
      val deserializedRequest = Serializer.deserialize[ CreateInvitationRequest ](serializedRequest)

      deserializedRequest match {
        case x: CreateInvitationRequest => {
          System.err.println("InvitationRequest before :" + sourceRequest)
          System.err.println("InvitationRequest after :" + x)
          x.toString must be_==(sourceRequest.toString)
          x.channelRole.toString must be_==(sourceRequest.channelRole.toString)
          x.channel.toString must be_==(sourceRequest.channel.toString)
          x must be_==(sourceRequest)
        }
        case _ => fail
      }


      val data = new PersistedMessage[ CreateInvitationRequest ](sourceRequest)
//      Serializer.evaluateSerializerClass(data) mustEqual ( KryoSerializer.getInstance().getClass.getName )
      val serialized = Serializer.serialize[ PersistedMessage[ CreateInvitationRequest ] ](data)
      val deserializedData = Serializer.deserialize[ PersistedMessage[ CreateInvitationRequest ] ](serialized)

      deserializedData match {
        case x: PersistedMessage[ CreateInvitationRequest ] => {

          x.id must be_==(data.id)
          System.err.println("InvitationRequest before :" + sourceRequest)
          System.err.println("InvitationRequest after :" + x.message)
          x.getMessage().toString must be_==(data.getMessage().toString)
          x.getMessage().channelRole.toString must be_==(sourceRequest.channelRole.toString)
          x must be_==(data)
        }
        case _ => fail
      }

    }



    "deserialize persisted GetContentRequest with PersistedMessage[ ReferralRequest ]" in {
      // skip("")

      val query: PersistedMessage[ ReferralRequest ] = new PersistedMessage[ ReferralRequest ]()

//      Serializer.evaluateSerializerClass(query) mustEqual ( KryoSerializer.getInstance().getClass.getName )
      val serialized = Serializer.serialize[ PersistedMessage[ ReferralRequest ] ](query)
      val deserializedData = Serializer.deserialize[ PersistedMessage[ ReferralRequest ] ](serialized)

      deserializedData match {
        case x: PersistedMessage[ ReferralRequest ] => {
          // empty message
          if ( x.getMessage() != null && query.getMessage() != null ) fail("")
          x must be_==(query)
          x.toString must be_==(query.toString)
        }
        case _ => fail
      }

    }
  }


  "smoke test size " should {
    val maxKB = 25 * 1024
    "not exceed 25KB for GetContent with Search" in {
      val queryObject = new Profile()
      queryObject.id = "123"
      queryObject.firstName = "testFirst"
      queryObject.lastName = "testLast"
      queryObject.description = "test Description"
      queryObject.emailAddress = "1@test.com"
      queryObject.country = "CA"
      queryObject.region = "Newfoundland and Labrador"
      queryObject.city = "St. John's"

      val msg = new GetContentRequest(null, queryObject)
      val serialized = Serializer.serialize[ GetContentRequest ](msg)

      serialized.length() must be < ( maxKB )
    }

    "not exceed 25KB for PersistedMessage" in {
      val inviteRequest = new InvitationRequest(new Identification(), new EventKey(UUID.randomUUID(), "tydegfrtew"), "IntroID", None, Some("Basic"), Some("Test Connection"), new Post() :: Nil)
      val data = new PersistedMessage[ InvitationRequest ](inviteRequest)
      val serialized = Serializer.serialize[ PersistedMessage[ InvitationRequest ] ](data)
      serialized.length() must be < ( maxKB )

    }

    "not exceed 5KB for connection" in {
      val data = new Connection("connectionType", ConnectionCategory.Self.toString, "alias", null, null, "autoApprove", List(ConnectionPolicy.ReferralsDisabled.toString, ConnectionPolicy.SearchDisabled.toString))
      val serialized = Serializer.serialize[ Connection ](data)
      serialized.length() must be < ( maxKB / 5 )

    }

  }

  val AVATAR_IMAGE_BYTE_REPRESENTATION = List[ Byte ](-1, -40, -1, -32, 0, 16, 74, 70, 73, 70, 0, 1, 1, 1, 0, 96, 0, 96, 0, 0, -1, -31, 0, 104, 69, 120, 105,
    102, 0, 0, 77, 77, 0, 42, 0, 0, 0, 8, 0, 4, 1, 26, 0, 5, 0, 0, 0, 1, 0, 0, 0, 62, 1, 27, 0, 5, 0, 0, 0,
    1, 0, 0, 0, 70, 1, 40, 0, 3, 0, 0, 0, 1, 0, 2, 0, 0, 1, 49, 0, 2, 0, 0, 0, 17, 0, 0, 0, 78, 0, 0, 0, 0, 0, 0, 0, 96, 0, 0, 0, 1, 0, 0, 0, 96, 0, 0, 0, 1, 80,
    -94, 46, -26, 58, -58, -103, 115, 111, 115, 5, -4, -52, 119, 59, -17, -128, -108, 61, 64, 3, 36, -123, 10, 50, 113, -102, -12, 122, -2, 30, 63, 102, 95, -8,
    40, 55, -59, -1, 0, -40, -97, -30, 40, -15, 15, -62, 127, 30, 107, -66, 13, -69, 18, -125, 52, 54, -109, -121, -76, -67, 10, -63, -79, 60, 7, 48, 76, -71, 3,
    33, -108, -125, -114, -107, -6, -17, -5, 11, -1, 0, -63, -24, -38, -74, -113, 29, -106, -115, -5, 65, -4, 62, -121, 89, -116, -78, -92, -98, 35, -16, -95, 22,
    -9, 8, -71, 57, 50, 89, -56, 118, 72, -36, -116, 121, 114, 70, 6, 58, 28, -26, -66, -86, 48, 81, -75, 56, 116, 62, 26, 82, -108, -65, 121, 62, -89, -1, -39).toArray[ Byte ]


}