package com.protegra_ati.agentservices.core.platformagents

import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import org.junit._
import Assert._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import java.net.{InetSocketAddress, URI}
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import invitation._
import scala.util.Random

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import java.util.{Locale, UUID}
import com.protegra_ati.agentservices.core.util.Results
class AgentHostCombinedInvitationsTest
  extends JUnit4(AgentHostCombinedInvitationsTestSpecs)

object AgentHostCombinedInvitationsTestSpecsRunner
  extends ConsoleRunner(AgentHostCombinedInvitationsTestSpecs)

object AgentHostCombinedInvitationsTestSpecs extends Specification
with Timeouts
with InvitationUser
with ReferralUser
{
  AgentHostCombinedInvitationsInitializer.setup(this)
  val uiR = AgentHostCombinedInvitationsInitializer.uiR
  val storeR = AgentHostCombinedInvitationsInitializer.storeR

  "Invitations" should {
    //"Invitations" ->- immutableTestDataSetupContext should {
    val brokerId = "Broker" + UUID.randomUUID()
    //    val brokerId = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
    //    val jasonId = UUID.fromString("dbd56858-adfe-4a12-a22c-f356dff4508d")
    val jasonId = "Jason" + UUID.randomUUID()
    val mikeId = "Mike" + UUID.randomUUID()


    "create connections from an accepted CreateInvitationRequest" in {
      val agentSessionIdBroker = UUID.randomUUID()
      val agentSessionIdMike = UUID.randomUUID()
      val agentSessionIdJason = UUID.randomUUID()
      val requestedCategory1 = ConnectionCategory.Person.toString
      val requestedCategory2 = ConnectionCategory.Person.toString
      val requestedConnectionType1 = "full"
      val requestedConnectionType2 = "custom"
      val requestedConnectionName1 = "New Mike"
      val requestedConnectionName2 = "New Jason"

      val eventKey = "invitation"

      //none of these are persisted to db so they don't affect the count
      val connBrokerBroker = AgentHostCombinedBase.setupConnection(storeR, brokerId, brokerId)
      storeR.addToHostedCnxn(connBrokerBroker.writeCnxn)

      val connJasonJason = AgentHostCombinedBase.setupConnection(storeR, jasonId, jasonId) //invitee
      storeR.addToHostedCnxn(connJasonJason.writeCnxn)

      val connJasonBroker = AgentHostCombinedBase.setupConnection(storeR, jasonId, brokerId)
      val connBrokerJason = AgentHostCombinedBase.setupConnection(storeR, brokerId, jasonId)

      val connMikeMike = AgentHostCombinedBase.setupConnection(storeR, mikeId, mikeId) //inviter
      storeR.addToHostedCnxn(connMikeMike.writeCnxn)

      val connMikeBroker = AgentHostCombinedBase.setupConnection(storeR, mikeId, brokerId)
      val connBrokerMike = AgentHostCombinedBase.setupConnection(storeR, brokerId, mikeId)

      // at the end connection between Mike and Jason has to be established
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerJason, null)
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerMike, null)

      AgentHostCombinedBase.setProfile(uiR, connJasonJason.writeCnxn, UUID.randomUUID(), eventKey, Locale.ENGLISH.toString(), createProfileForTestFull("jason", "klassen", "Jason.Klassen@protegra.com", "CA"))
      AgentHostCombinedBase.setProfile(uiR, connMikeMike.writeCnxn, UUID.randomUUID(), eventKey, Locale.ENGLISH.toString(), createProfileForTestFull("mike", "gevantmakher", "Mike.Gevantmakher@protegra.com", "US"))



      Thread.sleep(TIMEOUT_LONG)
      // Jason asks broker to be connected to Mikhaial
      val postToBroker = new Post("Hi Broker here is Mike, connect me to Jason", "theBody", new java.util.HashMap())
      val postToInvitee = new Post("Hi Jason here is Mike, nice to meet you", "theBody", new java.util.HashMap())
      // inveiter asks to be connected by broker and send 2 messages: 1 to the broker, 1 to the Invitee
      requestInvitation(uiR, connMikeBroker, connBrokerJason.id, connJasonJason.alias, connBrokerJason.alias, requestedCategory1, requestedConnectionType1, requestedConnectionName1, postToBroker, postToInvitee, agentSessionIdMike, eventKey)
      Thread.sleep(TIMEOUT_SHORT)
      // checks if invitation was processed on the store successfully
      assertInvitationResponse(uiR, agentSessionIdMike, eventKey);

      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      Thread.sleep(TIMEOUT_LONG * 5)
      // broker logs in, and see in his GUI all outstanding invitation requests.
      // he forwards the request, creating 2 referral requests to both parties, adds also own posts to both parties
      //Broker reads/compares initial message from the source of the conversation to him
      val postBroker2Target = new Post("Hi Target (Jason) here is a Broker", "theBody", new java.util.HashMap())
      val postBroker2Source = new Post("Hi Source (Mike) here is a Broker", "theBody", new java.util.HashMap())
      // simulates broker which is logged in and forwards manually the referral response to the stored referral request of the conversation initiator
      autoAcceptReferralRequest(uiR, connBrokerBroker, agentSessionIdBroker, eventKey, postBroker2Target, postBroker2Source, compareBrokerPostHandler(_: Post, postToBroker))
      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      Thread.sleep(TIMEOUT_LONG * 5)
      // simulates invitee (jason) which accepts InvitationRequest (after he is logged in and check his stored InvitationRequest list)
      autoAcceptInvitationRequest(uiR, connJasonJason, requestedCategory2, requestedConnectionType2, requestedConnectionName2, agentSessionIdJason, eventKey, compareInviteePostHandler(_: List[ Post ], List(postToInvitee, postBroker2Target)))
      // time for the connection creation
      Thread.sleep(TIMEOUT_LONG * 5)
      // checks established connections
      val query: Connection = new Connection();
      AgentHostCombinedBase.countConnectionsByType(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionType1) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByName(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionName1) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByType(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionType2) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByName(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionName2) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
    }

    "broker sends rejecting post & CreateInvitationRequest was not acepted, no connection will be created" in {
     skip("")
      val agentSessionIdBroker = UUID.randomUUID()
      val agentSessionIdMike = UUID.randomUUID()
      val agentSessionIdJason = UUID.randomUUID()
      val requestedCategory1 = ConnectionCategory.Person.toString
      val requestedCategory2 = ConnectionCategory.Person.toString
      val requestedConnectionType1 = "full"
      val requestedConnectionType2 = "custom"
      val requestedConnectionName1 = "New Mike"
      val requestedConnectionName2 = "New Jason"

      val eventKey = "invitation"


      //none of these are persisted to db so they don't affect the count
      val connBrokerBroker = AgentHostCombinedBase.setupConnection(storeR, brokerId, brokerId)
      storeR.addToHostedCnxn(connBrokerBroker.writeCnxn)

      val connJasonJason = AgentHostCombinedBase.setupConnection(storeR, jasonId, jasonId) //invitee
      storeR.addToHostedCnxn(connJasonJason.writeCnxn)

      val connJasonBroker = AgentHostCombinedBase.setupConnection(storeR, jasonId, brokerId)
      val connBrokerJason = AgentHostCombinedBase.setupConnection(storeR, brokerId, jasonId)

      val connMikeMike = AgentHostCombinedBase.setupConnection(storeR, mikeId, mikeId) //inviter
      storeR.addToHostedCnxn(connMikeMike.writeCnxn)

      val connMikeBroker = AgentHostCombinedBase.setupConnection(storeR, mikeId, brokerId)
      val connBrokerMike = AgentHostCombinedBase.setupConnection(storeR, brokerId, mikeId)

      // at the end connection between Mike and Jason has to be established
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerJason, null)
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerMike, null)
      Thread.sleep(TIMEOUT_LONG)
      // Jason asks broker to be connected to Mikhaial
      val postToBroker = new Post("Hi Broker here is Mike, connect me to Jason", "theBody", new java.util.HashMap())
      val postToInvitee = new Post("Hi Jason here is Mike, nice to meet you", "theBody", new java.util.HashMap())
      // inveiter asks to be connected by broker and send 2 messages: 1 to the broker, 1 to the Invitee
      requestInvitation(uiR, connMikeBroker, connBrokerJason.id, connJasonJason.alias, connBrokerJason.alias, requestedCategory1, requestedConnectionType1, requestedConnectionName1, postToBroker, postToInvitee, agentSessionIdMike, eventKey)
      Thread.sleep(TIMEOUT_SHORT)
      // checks if invitation was processed on the store successfully
      assertInvitationResponse(uiR, agentSessionIdMike, eventKey);

      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      Thread.sleep(TIMEOUT_LONG * 5)
      // broker logs in, and see in his GUI all outstanding invitation requests.
      //broker reads/compares initial message from the source of the conversation to him and rejects the request
      val postBroker2Source = new Post("Hi Source (Mike) here is a Broker, I can't connect you, sorry", "theBody", new java.util.HashMap())
      // simulates broker which is logged in and forwards manually the referral response to the stored referral request of the conversation initiator
      autoRejectReferralRequest(uiR, connBrokerBroker, agentSessionIdBroker, eventKey, postBroker2Source, compareBrokerPostHandler(_: Post, postToBroker))
      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      // time for the connection creation
      //      Thread.sleep(TIMEOUT_LONG * 5)
      // checks established connections
      val query: Connection = new Connection();
      // no connections should be created
      AgentHostCombinedBase.countConnectionsByType(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionType1) must be_==(0).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByName(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionName1) must be_==(0).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByType(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionType2) must be_==(0).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      AgentHostCombinedBase.countConnectionsByName(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionName2) must be_==(0).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      //      Thread.sleep(TIMEOUT_LONG)
      val eventKey1 = "content"
      // checks if rejecting post sent back from Broker and stored properli in conn brokerToInitiator
      AgentHostCombinedBase.count(uiR, connBrokerMike.writeCnxn, UUID.randomUUID(), eventKey1, new Post()) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
    }

    "invited person rejects the request, no connection will be created " in {
     skip("")
      val agentSessionIdBroker = UUID.randomUUID()
      val agentSessionIdMike = UUID.randomUUID()
      val agentSessionIdJason = UUID.randomUUID()
      val requestedCategory1 = ConnectionCategory.Person.toString
      val requestedCategory2 = ConnectionCategory.Person.toString
      val requestedConnectionType1 = "full"
      val requestedConnectionType2 = "custom"
      val requestedConnectionName1 = "New Mike"
      val requestedConnectionName2 = "New Jason"

      val eventKey = "invitation"

      //none of these are persisted to db so they don't affect the count
      val connBrokerBroker = AgentHostCombinedBase.setupConnection(storeR, brokerId, brokerId)
      storeR.addToHostedCnxn(connBrokerBroker.writeCnxn)

      val connJasonJason = AgentHostCombinedBase.setupConnection(storeR, jasonId, jasonId) //invitee
      storeR.addToHostedCnxn(connJasonJason.writeCnxn)

      val connJasonBroker = AgentHostCombinedBase.setupConnection(storeR, jasonId, brokerId)
      val connBrokerJason = AgentHostCombinedBase.setupConnection(storeR, brokerId, jasonId)

      val connMikeMike = AgentHostCombinedBase.setupConnection(storeR, mikeId, mikeId) //inviter
      storeR.addToHostedCnxn(connMikeMike.writeCnxn)

      val connMikeBroker = AgentHostCombinedBase.setupConnection(storeR, mikeId, brokerId)
      val connBrokerMike = AgentHostCombinedBase.setupConnection(storeR, brokerId, mikeId)

      // at the end connection between Mike and Jason has to be established
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerJason, null)
      storeR.updateData(connBrokerBroker.writeCnxn, connBrokerMike, null)
      Thread.sleep(TIMEOUT_LONG)
      // Jason asks broker to be connected to Mikhaial
      val postToBroker = new Post("Hi Broker here is Mike, connect me to Jason", "theBody", new java.util.HashMap())
      val postToInvitee = new Post("Hi Jason here is Mike, nice to meet you", "theBody", new java.util.HashMap())
      // inveiter asks to be connected by broker and send 2 messages: 1 to the broker, 1 to the Invitee
      requestInvitation(uiR, connMikeBroker, connBrokerJason.id, connJasonJason.alias, connBrokerJason.alias, requestedCategory1, requestedConnectionType1, requestedConnectionName1, postToBroker, postToInvitee, agentSessionIdMike, eventKey)
      Thread.sleep(TIMEOUT_SHORT)
      // checks if invitation was processed on the store successfully
      assertInvitationResponse(uiR, agentSessionIdMike, eventKey);

      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      Thread.sleep(TIMEOUT_LONG * 5)
      // broker logs in, and see in his GUI all outstanding invitation requests.
      // he forwards the request, creating 2 referral requests to both parties, adds also own posts to both parties
      //Broker reads/compares initial message from the source of the conversation to him
      val postBroker2Target = new Post("Hi Target (Jason) here is a Broker", "theBody", new java.util.HashMap())
      val postBroker2Source = new Post("Hi Source (Mike) here is a Broker", "theBody", new java.util.HashMap())
      // simulates broker which is logged in and forwards manually the referral response to the stored referral request of the conversation initiator
      autoAcceptReferralRequest(uiR, connBrokerBroker, agentSessionIdBroker, eventKey, postBroker2Target, postBroker2Source, compareBrokerPostHandler(_: Post, postToBroker))
      //store bypasses waiters. so if the get happens before the store persisted we're out of luck
      Thread.sleep(TIMEOUT_LONG * 5)
      // simulates invitee (jason) which rejects InvitationRequest (after he is logged in and check his stored InvitationRequest list)
      autoRejectInvitationRequest(uiR, connJasonJason, requestedCategory2, requestedConnectionType2, requestedConnectionName2, agentSessionIdJason, eventKey, compareInviteePostHandler(_: List[ Post ], List(postToInvitee, postBroker2Target)), "thank you better not ")
      // time for the connection creation
      Thread.sleep(TIMEOUT_LONG)
      // checks established connections
      val query: Connection = new Connection();
      AgentHostCombinedBase.countConnectionsByType(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionType1) must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
      AgentHostCombinedBase.countConnectionsByName(uiR, connMikeMike.writeCnxn, agentSessionIdMike, eventKey, query, requestedConnectionName1) must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
      AgentHostCombinedBase.countConnectionsByType(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionType2) must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
      AgentHostCombinedBase.countConnectionsByName(uiR, connJasonJason.writeCnxn, agentSessionIdJason, eventKey, query, requestedConnectionName2) must be_==(0).eventually(10, TIMEOUT_EVENTUALLY)
      val eventKey1 = "content"
      // checks if rejecting post sent back from Broker and stored properli in conn brokerToInitiator
      AgentHostCombinedBase.count(uiR, connJasonBroker.writeCnxn, UUID.randomUUID(), eventKey1, new Post()) must be_==(1).eventually(10, TIMEOUT_EVENTUALLY)
      // also in jason self one post has to be stored
      AgentHostCombinedBase.count(uiR, connJasonJason.readCnxn, UUID.randomUUID(), eventKey1, new Post()) must be_==(1).eventually(10, TIMEOUT_EVENTUALLY)
    }

    def compareBrokerPostHandler(receivedPostToBroker: Post, expectedPostToBroker: Post): Unit =
    {
      if ( expectedPostToBroker != null ) {
        if ( receivedPostToBroker == null ) fail("message to the inviter->broker is not properly transmitted")
        if ( !receivedPostToBroker.equals(expectedPostToBroker) ) fail("message to the inviter->broker is not properly transmitted")
      }
    }

    def compareInviteePostHandler(receivedInviteePosts: List[ Post ], expectedInviteePosts: List[ Post ]): Unit =
    {
      if ( !receivedInviteePosts.equals(expectedInviteePosts) ) fail("posts won't be transferred properly to the invitee")
    }

    def requestInvitation(ui: AgentHostUIPlatformAgent, connInviterBroker: Connection, targetConnectionId: String, selfAlias: String, targetAlias: String, requestedCategory: String, requestedConnectionType: String, requestedConnectionName: String, postToBroker: Post, postToInvitee: Post, agentSessionId: UUID, tag: String) =
    {
      val req = new CreateInvitationRequest(new EventKey(agentSessionId, tag), targetConnectionId.toString, selfAlias, targetAlias, requestedCategory, requestedConnectionType, requestedConnectionName, postToInvitee, postToBroker)
      // TODO add to the constructor to prevent errors !!!
      req.targetCnxn = connInviterBroker.readCnxn
      ui.send(req)
    }

    def assertInvitationResponse(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) =
    {
      val key = Results.getKey()
      ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
      {
        override def createInvitationResponseReceived(e: CreateInvitationResponseReceivedEvent) =
        {

          println("***********invitation response recived ---------------------------:" + e.toString)
          e.msg.status.toLowerCase() match {
            case "success" => Results.trigger(key)
            case _ => {}
          }
        }

      });

      Results.triggered(key) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }

  }

  def createProfileForTestFull(name: String, lastName: String, email: String, country: String): Profile =
  {
    new Profile(name, lastName, "test Description", email, country, "someprovince", "city", "postalCode", "website")
  }


}

object AgentHostCombinedInvitationsInitializer extends Specification with
Timeouts
{

  AgentHostCombinedBase.setup(this)
  val uiR = AgentHostCombinedBase.uiRef
  val storeR = AgentHostCombinedBase.storeRef

  val setup = new SpecContext
  {}
}
