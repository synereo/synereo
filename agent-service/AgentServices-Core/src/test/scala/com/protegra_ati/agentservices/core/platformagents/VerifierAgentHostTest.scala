//package com.protegra_ati.agentservices.core.platformagents
//
//import org.specs._
//import org.specs.util._
//import org.specs.runner._
//import org.junit._
//import java.util.UUID
//import java.net.URI
//
//import com.protegra.agentservicesstore.extensions.StringExtensions._
//import com.protegra.agentservicesstore.extensions.URIExtensions._
//import com.protegra_ati.agentservices.core.messages._
//import com.protegra.agentservicesstore.usage.AgentKVDBScope._
//import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
//import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
//import java.net.URI
//import content.{GetContentResponse, SetContentPersistedRequest}
//import verifier._
//import org.joda.time.{DateTime, Instant}
//import com.protegra.agentservicesstore._
//import com.protegra_ati.agentservices.core.events._
//import com.protegra_ati.agentservices.core.schema._
//import com.protegra_ati.agentservices.core._
//import com.protegra_ati.agentservices.core.schema.util.{ConnectionFactory}
//import com.protegra_ati.agentservices.core.schema.{PersistedRequest, ContentVerifier, Profile, Verifier, Data, CompositeData, AliasConnection, VerifiedData}
//
//class VerifierAgentHostTest
//  extends JUnit4(VerifierAgentHostTestSpecs)
//
//object VerifierAgentHostTestSpecsRunner
//  extends ConsoleRunner(VerifierAgentHostTestSpecs)
//
//object VerifierAgentHostTestSpecs extends Specification
//with RabbitTestSetup
//with Timeouts
//{
//
//  var cnxnRandom = new AgentCnxnProxy("CombinedTest".toURI, "", "User".toURI)
//
//  val relyingAgentId = "RelyingAgent" + UUID.randomUUID.toString
//  val claimingAgentId = "ClaimingAgent" + UUID.randomUUID.toString
//  val verifierId = "Verifier" + UUID.randomUUID.toString
//
//  val cnxnCASelf = AgentCnxnProxy(claimingAgentId.toURI, "", claimingAgentId.toURI)
//  val cnxnRASelf = AgentCnxnProxy(relyingAgentId.toURI, "", relyingAgentId.toURI)
//  val cnxnVerifierSelf = AgentCnxnProxy(verifierId.toURI, "", verifierId.toURI)
//
//  AgentHostCombinedBase.setup(this)
//  val uiPA = AgentHostCombinedBase.uiRef
//  val storePA = AgentHostCombinedBase.storeRef
//
//
//
//
//
//  //  def createVerifier():AgentHostStorePlatformAgent =
//  //  {
//  //    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_Verifier)
//  //    val acquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_CLAIMING_AGENT),"127.0.0.1".toURI.withPort(RABBIT_PORT_RELYING_AGENT))
//  //
//  //    val pa = new AgentHostStorePlatformAgent()
//  //    pa.init(sourceAddress, acquaintanceAddresses, UUID.randomUUID)
//  //    //init must happen before listening
//  //    Thread.sleep(TIMEOUT_MED)
//  //    pa
//  //  }
//
//  "Sending a SetContentPersistedRequest for a persisted SelectVerifierRequest message" should {
//    skip("listen issues")
//    //skip("don't run unless all three rabbits are running")
//    val agentSessionId = UUID.randomUUID()
//
//
//    var triggered = false
//    var getClaimResponse: GetClaimResponse = null
//
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnCARA = AgentCnxnProxy(claimingAgentId.toURI, "", relyingAgentId.toURI)
//
//    val verifier = Verifier("Government of Manitoba")
//    val verifiers = List(verifier)
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", ConnectionCategory.Self.toString, "Full", claimingAgentId, relyingAgentId, "false", List())
//    storePA.store(storePA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//
//    //store a CA/Verifier connection on the CA self connection
//    val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba", ConnectionCategory.Self.toString, "Full", claimingAgentId, verifierId, "false", List("verifier"))
//    storePA.store(storePA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//    Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    storePA.listenForHostedCnxns
//
//    val profile = new Profile("John", "Smith", "test Description", "12345@someProvider.au", "AU", "someAUterritory", "city", "postalCode", "website")
//    storePA.store(storePA._dbQ, cnxnRACA, profile.toStoreKey, Serializer.serialize[ Data ](profile))
//
//    val contentVerifier = ContentVerifier(connCAVerifier.id, "GoM", "profile.lastName", "applicant1", verifier.id, "true", "APPROVED")
//    storePA.store(storePA._dbQ, cnxnRACA, contentVerifier.toStoreKey, Serializer.serialize[ Data ](contentVerifier))
//
//    // specs error
//    "trigger a GetClaimResponse message" in {
//      uiPA.addListener(agentSessionId, "", new MessageEventAdapter("persistedContent")
//      {
//        override def getClaimResponseReceived(e: GetClaimResponseReceivedEvent) =
//        {
//          triggered = true
//          e.msg match {
//            case x: GetClaimResponse => {
//              getClaimResponse = x
//            }
//            case _ => {}
//          }
//        }
//      });
//
//      var request = SelectVerifierRequest(new Identification(), new EventKey(agentSessionId, "persistedContent"), verifiers, "profile", "lastName", "Bank of Montreal")
//      val persistedRequest = PersistedRequest("SelectVerifierRequest", request, new DateTime())
//      request.targetCnxn = cnxnCARA
//      request.originCnxn = storePA._cnxnUIStore
//      storePA.store(storePA._dbQ, cnxnCARA, persistedRequest.toStoreKey, Serializer.serialize[ Data ](persistedRequest))
//      Thread.sleep(TIMEOUT_LONG)
//
//      val response = SelectVerifierResponse(request.ids.copyAsChild(), request.eventKey.copy(), "profile", "lastName", verifier)
//      response.targetCnxn = cnxnRACA
//      response.originCnxn = request.originCnxn
//      val conn = ConnectionFactory.createConnection("CARA", ConnectionCategory.Person.toString, "Full", claimingAgentId, relyingAgentId)
//      val compositeData = new CompositeData[ PersistedRequest ](conn, persistedRequest)
//      val msg = SetContentPersistedRequest(new EventKey(agentSessionId, "persistedContent"), compositeData, response, true)
//      msg.targetCnxn = cnxnCARA
//      msg.originCnxn = storePA._cnxnUIStore
//      uiPA.send(msg)
//      Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = GetClaimResponse(request.ids.copyAsChild(), null, "profile", "lastName", verifier)
//
//      expectedMsg.ids.conversationId must be_==(getClaimResponse.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimObject must be_==(getClaimResponse.claimObject).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimField must be_==(getClaimResponse.claimField).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.verifier must be_==(getClaimResponse.verifier).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//  }
//
//  "Sending a VerifyRequest message" should {
//    skip("listen issues")
//    //     skip("don't run unless all three rabbits are running")
//
//    val agentSessionId = UUID.randomUUID()
//
//
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnCARA = AgentCnxnProxy(claimingAgentId.toURI, "", relyingAgentId.toURI)
//    val cnxnRAVerifier = AgentCnxnProxy(relyingAgentId.toURI, "", verifierId.toURI)
//    val cnxnVerifierRA = AgentCnxnProxy(verifierId.toURI, "", relyingAgentId.toURI)
//    val cnxnCAVerifier = AgentCnxnProxy(claimingAgentId.toURI, "", verifierId.toURI)
//
//    val verifier = Verifier("Government of Manitoba")
//    val verifiers = List(verifier)
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", ConnectionCategory.Self.toString, "Full", claimingAgentId, relyingAgentId, "false", List())
//    storePA.store(storePA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//
//    //store a CA/Verifier connection on the CA self connection
//    val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba",  ConnectionCategory.Self.toString, "Full", claimingAgentId, verifierId, "false", List("verifier"))
//    storePA.store(storePA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//    Thread.sleep(TIMEOUT_LONG)
//
//    //store a Verifier/CA connection on the Verifier self connection
//    val connVerifierCA = ConnectionFactory.createConnection("applicant1", ConnectionCategory.Self.toString, "ClaimingAgent", verifierId, claimingAgentId)
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, connVerifierCA.toStoreKey, Serializer.serialize[ Data ](connVerifierCA))
//
//    //store a Verifier/RA connection on the Verifier self connection
//    val connVerifierRA = ConnectionFactory.createConnection("RA", ConnectionCategory.Self.toString, "RelyingAgent", verifierId, relyingAgentId)
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, connVerifierRA.toStoreKey, Serializer.serialize[ Data ](connVerifierRA))
//    Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    storePA.listenForHostedCnxns
//
//    var aliasConn = AliasConnection("applicant1", connVerifierCA.id)
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, aliasConn.toStoreKey, Serializer.serialize[ Data ](aliasConn))
//
//    val verifiedSalary = VerifiedData("applicant1", "salary", "50000")
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, verifiedSalary.toStoreKey, Serializer.serialize[ Data ](verifiedSalary))
//
//    val verifiedLastName = VerifiedData("applicant1", "lastName", "Smith")
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, verifiedLastName.toStoreKey, Serializer.serialize[ Data ](verifiedLastName))
//
//    val contentVerifierLastName = ContentVerifier(connVerifierCA.id, "BMO", "profile.lastName", "applicant1", verifier.id, "true", "APPROVED")
//    storePA.store(storePA._dbQ, cnxnCAVerifier, contentVerifierLastName.toStoreKey, Serializer.serialize[ Data ](contentVerifierLastName))
//
//    val contentVerifierSalary = ContentVerifier(connVerifierCA.id, "BMO", "profile.salary", "applicant1", verifier.id, "false", "APPROVED")
//    storePA.store(storePA._dbQ, cnxnCAVerifier, contentVerifierSalary.toStoreKey, Serializer.serialize[ Data ](contentVerifierSalary))
//
//    val contentVerifierBirthDate = ContentVerifier(connVerifierCA.id, "BMO", "profile.birthDate", "applicant1", verifier.id, "true", "APPROVED")
//    storePA.store(storePA._dbQ, cnxnCAVerifier, contentVerifierBirthDate.toStoreKey, Serializer.serialize[ Data ](contentVerifierBirthDate))
//
//    var verifyResponse: VerifyResponse = null
//    var verifyPermissionRequest: VerifyPermissionRequest = null
//    var triggered = false
//
//    def handleVerifyPermissionRequest(cnxn: AgentCnxnProxy, msg: Message) =
//    {
//      msg match {
//        case x: VerifyPermissionRequest => {
//          verifyPermissionRequest = x
//        }
//        case _ => {}
//      }
//      triggered = true
//    }
//
//    def handleVerifyResponse(cnxn: AgentCnxnProxy, msg: Message) =
//    {
//      msg match {
//        case x: VerifyResponse => {
//          verifyResponse = x
//        }
//        case _ => {}
//      }
//      triggered = true
//    }
//
//    def fetchData(queue: Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn: AgentCnxnProxy, searchKey: String): VerifyPermissionRequest =
//    {
//      storePA.fetch[ Data ](queue, cnxn, searchKey, handleFetch)
//      return verifyPermissionRequest
//    }
//
//    def handleFetch(cnxn: AgentCnxnProxy, data: Data) =
//    {
//      data match {
//        case x: PersistedRequest => {
//          x.message match {
//            case request: VerifyPermissionRequest => {
//              verifyPermissionRequest = request
//            }
//            case _ => {}
//          }
//        }
//        case _ => {}
//      }
//    }
//
//    // specs error
//    "trigger a persisted VerifyPermissionRequest when content is not auto-approved" in {
//      var msg: VerifyRequest = new VerifyRequest(new Identification(), null, "applicant1", "salary", "50000", "Loan application", "Bank of Montreal")
//      msg.originCnxn = cnxnRAVerifier
//      msg.targetCnxn = cnxnVerifierRA
//      uiPA.send(msg)
//      Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = VerifyPermissionRequest(msg.ids.copyAsChild(), null, msg.claimKey, msg.claimData, msg.reason, msg.relyingAgentDescription)
//
//      val search = new PersistedRequest("VerifyPermissionRequest", null, null)
//      verifyPermissionRequest = fetchData(storePA._dbQ, cnxnCAVerifier, search.toSearchKey)
//      Thread.sleep(TIMEOUT_LONG)
//
//      expectedMsg.ids.conversationId must be_==(verifyPermissionRequest.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimKey must be_==(verifyPermissionRequest.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimData must be_==(verifyPermissionRequest.claimData).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.reason must be_==(verifyPermissionRequest.reason).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.relyingAgentDescription must be_==(verifyPermissionRequest.relyingAgentDescription).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//
//    "trigger a VerifyResponse message when content is auto-approved" in {
//      uiPA.addListener(agentSessionId, "", new MessageEventAdapter("verifyRequest")
//      {
//        override def verifyResponseReceived(e: VerifyResponseReceivedEvent) =
//        {
//          triggered = true
//          e.msg match {
//            case x: VerifyResponse => {
//              verifyResponse = x
//            }
//            case _ => {}
//          }
//        }
//      });
//
//      var msg: VerifyRequest = new VerifyRequest(new Identification(), EventKey(agentSessionId, "verifyRequest"), "applicant1", "lastName", "Smith", "?", "Bank of Montreal")
//      msg.originCnxn = cnxnRAVerifier
//      msg.targetCnxn = cnxnVerifierRA
//      uiPA.send(msg)
//      Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = VerifyResponse(msg.ids.copyAsChild(), msg.eventKey, msg.alias, msg.claimKey, true)
//
//      triggered must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.ids.conversationId must be_==(expectedMsg.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.alias must be_==(expectedMsg.alias).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.claimKey must be_==(expectedMsg.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.isVerified must be_==(expectedMsg.isVerified).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//
//    "trigger a verify response of false when verified data does not match claim" in {
//      uiPA.addListener(agentSessionId, "", new MessageEventAdapter("verifyRequestDeny")
//      {
//        override def verifyResponseReceived(e: VerifyResponseReceivedEvent) =
//        {
//          triggered = true
//          e.msg match {
//            case x: VerifyResponse => {
//              verifyResponse = x
//            }
//            case _ => {}
//          }
//        }
//      });
//
//      var msg: VerifyRequest = new VerifyRequest(new Identification(), EventKey(agentSessionId, "verifyRequestDeny"), "applicant1", "lastName", "Jones", "?", "Bank of Montreal")
//      msg.originCnxn = cnxnRAVerifier
//      msg.targetCnxn = cnxnVerifierRA
//      uiPA.send(msg)
//      Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = VerifyResponse(msg.ids.copyAsChild(), msg.eventKey, msg.alias, msg.claimKey, false)
//
//      triggered must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.ids.conversationId must be_==(expectedMsg.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.alias must be_==(expectedMsg.alias).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.claimKey must be_==(expectedMsg.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//      verifyResponse.isVerified must be_==(expectedMsg.isVerified).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//  }
//
//  "Sending a SetContentPersistedRequest for a persisted VerifyPermissionRequest message" should {
//    skip("listen issues")
//    //skip("don't run unless all three rabbits are running")
//    val agentSessionId = UUID.randomUUID()
//
//
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnVerifierCA = AgentCnxnProxy(verifierId.toURI, "", claimingAgentId.toURI)
//    val cnxnCAVerifier = AgentCnxnProxy(claimingAgentId.toURI, "", verifierId.toURI)
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", ConnectionCategory.Self.toString, "Full", claimingAgentId, relyingAgentId, "false", List())
//    storePA.store(storePA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//
//    //store a CA/Verifier connection on the CA self connection
//    val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba", ConnectionCategory.Self.toString, "Full", claimingAgentId, verifierId, "false", List("verifier"))
//    storePA.store(storePA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//    Thread.sleep(TIMEOUT_LONG)
//
//    //store a Verifier/CA connection on the Verifier self connection
//    val connVerifierCA = ConnectionFactory.createConnection("applicant1", ConnectionCategory.Self.toString, "ClaimingAgent", verifierId, claimingAgentId)
//    storePA.store(storePA._dbQ, cnxnVerifierSelf, connVerifierCA.toStoreKey, Serializer.serialize[ Data ](connVerifierCA))
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    storePA.listenForHostedCnxns
//
//    val verifier = Verifier("Government of Manitoba")
//    val verifiers = List(verifier)
//
//    val profile =new Profile("John", "Smith", "test Description", "12345@someProvider.au", "AU", "someAUterritory", "city", "postalCode", "website")
//    storePA.store(storePA._dbQ, cnxnRACA, profile.toStoreKey, Serializer.serialize[ Data ](profile))
//
//    val contentVerifier = ContentVerifier(connCAVerifier.id, "GoM", "profile.lastName", "applicant1", verifier.id, "true", "APPROVED")
//    storePA.store(storePA._dbQ, cnxnRACA, contentVerifier.toStoreKey, Serializer.serialize[ Data ](contentVerifier))
//
//    var verifyPermissionResponse: VerifyPermissionResponse = null
//
//    def handleVerifyPermissionResponse(cnxn: AgentCnxnProxy, msg: Message) =
//    {
//      msg match {
//        case x: VerifyPermissionResponse => {
//          verifyPermissionResponse = x
//        }
//        case _ => {}
//      }
//    }
//
//    "trigger a VerifyPermissionResponse message" in {
//      var request = VerifyPermissionRequest(new Identification(), new EventKey(agentSessionId, "verifyPermissionRequest"), "salary", "50000", "Loan application", "Bank of Montreal")
//      request.targetCnxn = cnxnCAVerifier
//      request.originCnxn = cnxnVerifierCA
//      request.channelLevel = Some(ChannelLevel.Public)
//      val persistedRequest = PersistedRequest("VerifyPermissionRequest", request, new DateTime())
//      storePA.store(storePA._dbQ, cnxnCAVerifier, persistedRequest.toStoreKey, Serializer.serialize[ Data ](persistedRequest))
//      Thread.sleep(TIMEOUT_LONG)
//
//      val conn = ConnectionFactory.createConnection("CAVerifier", ConnectionCategory.Self.toString, "Full", claimingAgentId, verifierId)
//      val compositeData = new CompositeData[ PersistedRequest ](conn, persistedRequest)
//      val msg = SetContentPersistedRequest(new EventKey(agentSessionId, "verifyPermissionRequest"), compositeData, null, true)
//      msg.targetCnxn = cnxnCAVerifier
//      uiPA.send(msg)
//      Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = VerifyPermissionResponse(request.ids.copyAsChild, true)
//
//      expectedMsg.ids.conversationId must be_==(verifyPermissionResponse.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.isPermissionGranted must be_==(verifyPermissionResponse.isPermissionGranted).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//  }
//}
//
