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
//import content.SetContentPersistedRequest
//import verifier._
//import org.joda.time.{DateTime, Instant}
//import com.protegra_ati.agentservices.core.schema._
//import com.protegra_ati.agentservices.core.events.{GetClaimResponseReceivedEvent, GetContentResponseReceivedEvent, SetContentResponseReceivedEvent, MessageEventAdapter}
//
//class VerifierPlatformAgentTest
//extends JUnit4(VerifierPlatformAgentTestSpecs)
//
//object VerifierPlatformAgentTestSpecsRunner
//extends ConsoleRunner(VerifierPlatformAgentTestSpecs)
//
//object VerifierPlatformAgentTestSpecs extends Specification {
//  val TIMEOUT_SHORT = 100
//  val TIMEOUT_MED = 300
//  val TIMEOUT_LONG = 750
//  val TIMEOUT_EVENTUALLY = new Duration(1500)
//
//  val RABBIT_PORT_Verifier = 5555
//  val RABBIT_PORT_CLAIMING_AGENT = 5672
//  val RABBIT_PORT_RELYING_AGENT = 4444
//
//  val RABBIT_PORT_STORE = 5672
//  val RABBIT_PORT_UI = 5555
//
//  def createVerifier():AgentHostStorePlatformAgent =
//  {
//    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_Verifier)
//    val acquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_CLAIMING_AGENT),"127.0.0.1".toURI.withPort(RABBIT_PORT_RELYING_AGENT))
//
//    val pa = new AgentHostStorePlatformAgent()
//    pa.init(sourceAddress, acquaintanceAddresses, UUID.randomUUID)
//    //init must happen before listening
//    //Thread.sleep(TIMEOUT_MED)
//    pa
//  }
//
//  def createClaimingAgent(selfConnections:List[AgentCnxnProxy]): AgentHostStorePlatformAgent =
//  {
//    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_CLAIMING_AGENT)
//    val acquaintanceAddresses = List[ URI ]()
//    val publicAcquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_Verifier))
//    val pa = new AgentHostStorePlatformAgent()
//    pa._cnxnUserSelfConnectionsList = selfConnections
//    pa.init(sourceAddress, acquaintanceAddresses, publicAcquaintanceAddresses, UUID.randomUUID)
//    //Thread.sleep(TIMEOUT_MED)
//    pa
//  }
//
//  def createRelyingAgent(selfConnections:List[AgentCnxnProxy]): AgentHostStorePlatformAgent =
//  {
//    val sourceAddress = "127.0.0.1".toURI.withPort(RABBIT_PORT_RELYING_AGENT)
//    val acquaintanceAddresses = List[ URI ]()
//    val publicAcquaintanceAddresses = List[ URI ]("127.0.0.1".toURI.withPort(RABBIT_PORT_Verifier))
//    val pa = new AgentHostStorePlatformAgent()
//    pa._cnxnUserSelfConnectionsList = selfConnections
//    pa.init(sourceAddress, acquaintanceAddresses, publicAcquaintanceAddresses, UUID.randomUUID)
//    //Thread.sleep(TIMEOUT_MED)
//    pa
//  }
//
//  "Sending a GetClaimRequest message" should {
//    //skip("don't run unless all three rabbits are running")
//
//    val agentSessionId = UUID.randomUUID
//    val relyingAgentId = "RelyingAgent" + UUID.randomUUID.toString
//    val claimingAgentId = "ClaimingAgent" + UUID.randomUUID.toString
//
//    val cnxnCASelf = AgentCnxnProxy(claimingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnRASelf = AgentCnxnProxy(relyingAgentId.toURI, "", relyingAgentId.toURI)
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnCARA = AgentCnxnProxy(claimingAgentId.toURI, "", relyingAgentId.toURI)
//
//    val relyingAgentPA = createRelyingAgent(List(cnxnRASelf,cnxnCASelf))
//    val claimingAgentPA = createClaimingAgent(List(cnxnRASelf, cnxnCASelf))
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", "Full", claimingAgentId, relyingAgentId)
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    claimingAgentPA.listenForHostedCnxns
//
//    //store an RA/CA connection on the RA self connection
//    var connRACA = ConnectionFactory.createConnection("CA", "Full", relyingAgentId, claimingAgentId)
//    relyingAgentPA.store(relyingAgentPA._dbQ, cnxnRASelf, connRACA.toStoreKey, Serializer.serialize[ Data ](connRACA))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on relying agent PA - will start listening on the connections stored above on the relying agent pa self connection
//    relyingAgentPA.listenForHostedCnxns
//
//    var selectVerifierRequest:SelectVerifierRequest = null
//
//    def fetchData(queue:Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn:AgentCnxnProxy, searchKey:String):SelectVerifierRequest = {
//      claimingAgentPA.fetch[Data](queue, cnxn, searchKey, handleFetch)
//      return selectVerifierRequest
//    }
//
//    def handleFetch(cnxn: AgentCnxnProxy, data: Data) =
//    {
//      data match {
//        case x: PersistedRequest => {
//          x.message match {
//            case request:SelectVerifierRequest => {
//              selectVerifierRequest = request
//             }
//            case _ => {}
//          }
//        }
//        case _ => {}
//      }
//    }
//
//    "trigger a persisted SelectVerifierRequest" in {
//      val verifiers = List(Verifier( "Government of Manitoba"))
//      var msg = GetClaimRequest(new Identification(), new EventKey(agentSessionId, "claimRequest"), "profile", "lastName", verifiers, "Bank of Montreal")
//      msg.targetCnxn = cnxnCARA
//      msg.originCnxn = cnxnRACA
//      msg.channelLevel = Some(ChannelLevel.Public)
//      relyingAgentPA.send(relyingAgentPA._msgQ, cnxnCARA, msg)
//      //Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = SelectVerifierRequest(msg.ids.copyAsChild(), null, verifiers, msg.claimObject, msg.claimField, msg.relyingAgentDescription)
//
//      //Thread.sleep(TIMEOUT_LONG)
//      val search = new Search[PersistedRequest](classOf[PersistedRequest])
//      search.setSearchFieldValue("messageType", "SelectVerifierRequest")
//      selectVerifierRequest = fetchData(claimingAgentPA._dbQ, cnxnCARA, search.toSearchKey)
//      //Thread.sleep(TIMEOUT_LONG)
//
//      expectedMsg.ids.conversationId must be_==(selectVerifierRequest.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.verifierList must be_==(selectVerifierRequest.verifierList).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimObject must be_==(selectVerifierRequest.claimObject).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.claimField must be_==(selectVerifierRequest.claimField).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.relyingAgentDescription must be_==(selectVerifierRequest.relyingAgentDescription).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//  }
//
//  "Sending a SetContentPersistedRequest for a persisted SelectVerifierRequest message" should {
//    //skip("don't run unless all three rabbits are running")
//    val agentSessionId = UUID.randomUUID()
//
//    val relyingAgentId = "RelyingAgent" + UUID.randomUUID.toString
//    val claimingAgentId = "ClaimingAgent" + UUID.randomUUID.toString
//    val verifierId = "Verifier" + UUID.randomUUID.toString
//
//    val cnxnCASelf = AgentCnxnProxy(claimingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnRASelf = AgentCnxnProxy(relyingAgentId.toURI, "", relyingAgentId.toURI)
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnCARA = AgentCnxnProxy(claimingAgentId.toURI, "", relyingAgentId.toURI)
//
//    val relyingAgentPA = createRelyingAgent(List(cnxnRASelf,cnxnCASelf))
//    val claimingAgentPA = createClaimingAgent(List(cnxnRASelf, cnxnCASelf))
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", "Full", claimingAgentId, relyingAgentId)
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//
//    //store a CA/RA connection on the CA self connection
//    val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba", "Full", claimingAgentId, verifierId, false, List("verifier"))
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    claimingAgentPA.listenForHostedCnxns
//
//    //store an RA/CA connection on the RA self connection
//    var connRACA = ConnectionFactory.createConnection("CA", "Full", relyingAgentId, claimingAgentId)
//    relyingAgentPA.store(relyingAgentPA._dbQ, cnxnRASelf, connRACA.toStoreKey, Serializer.serialize[ Data ](connRACA))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on relying agent PA - will start listening on the connections stored above on the relying agent pa self connection
//    relyingAgentPA.listenForHostedCnxns
//
//    val verifier = Verifier( "Government of Manitoba")
//    val verifiers = List(verifier)
//
//    val profile = Profile( "John", "Smith", "12345")
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnRACA, profile.toStoreKey, Serializer.serialize[Data](profile))
//
//    val contentVerifier = ContentVerifier( connCAVerifier.id, "GoM", "profile.lastName", "applicant1", verifier.id, true, "APPROVED")
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnRACA, contentVerifier.toStoreKey, Serializer.serialize[Data](contentVerifier))
//
//    var getClaimResponse:GetClaimResponse = null
//
//     def handleGetClaimResponse(cnxn:AgentCnxnProxy, msg:Message) = {
//       msg match {
//         case x:GetClaimResponse => {
//           getClaimResponse = x
//         }
//         case _ => {}
//       }
//     }
//
//// specs error
//    "trigger a GetClaimResponse message" in {
//      var request = SelectVerifierRequest(new Identification(), new EventKey(agentSessionId, "claimRequest"), verifiers, "profile", "lastName", "Bank of Montreal")
//      val persistedRequest = PersistedRequest( "SelectVerifierRequest", request, new DateTime())
//
//      request.targetCnxn = cnxnCARA
//      request.originCnxn = claimingAgentPA._cnxnUIStore
//      claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCARA, persistedRequest.toStoreKey, Serializer.serialize[Data](persistedRequest))
//      //Thread.sleep(TIMEOUT_LONG)
//
//      val response = SelectVerifierResponse(request.ids.copyAsChild(), request.eventKey.copy(), "profile", "lastName", verifier)
//      response.targetCnxn = cnxnRACA
//      response.originCnxn = request.originCnxn
//      val conn = ConnectionFactory.createConnection("CARA", "Full", claimingAgentId, relyingAgentId)
//      val compositeData = new CompositeData[PersistedRequest](conn, persistedRequest)
//      val msg = SetContentPersistedRequest(new EventKey(agentSessionId, "verifierSelected"), compositeData, response, true)
//      msg.targetCnxn = cnxnCARA
//      msg.originCnxn = claimingAgentPA._cnxnUIStore
//      msg.channelLevel = Some(ChannelLevel.Public)
//      claimingAgentPA.send(claimingAgentPA._msgQ, cnxnCARA, msg)
//      //Thread.sleep(TIMEOUT_LONG)
//
//      relyingAgentPA.listen(relyingAgentPA._msgQ, cnxnRACA, Channel.Verify, ChannelType.Response, ChannelLevel.Public, handleGetClaimResponse(_:AgentCnxnProxy, _:Message))
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
//   "Sending a VerifyRequest message" should {
////     skip("don't run unless all three rabbits are running")
//
//     val verifierId = "Verifier" + UUID.randomUUID.toString
//     val relyingAgentId = "RelyingAgent" + UUID.randomUUID.toString
//     val claimingAgentId = "ClaimingAgent" + UUID.randomUUID.toString
//
//     val cnxnRAVerifier = AgentCnxnProxy(relyingAgentId.toURI, "", verifierId.toURI)
//     val cnxnVerifierRA = AgentCnxnProxy(verifierId.toURI, "", relyingAgentId.toURI)
//     val cnxnCAVerifier = AgentCnxnProxy(claimingAgentId.toURI, "", verifierId.toURI)
//     val cnxnCASelf = AgentCnxnProxy(claimingAgentId.toURI, "", claimingAgentId.toURI)
//     val cnxnRASelf = AgentCnxnProxy(relyingAgentId.toURI, "", relyingAgentId.toURI)
//
//     val relyingAgentPA = createRelyingAgent(List(cnxnRASelf,cnxnCASelf))
//     val claimingAgentPA = createClaimingAgent(List(cnxnRASelf, cnxnCASelf))
//     var verifierPA = createVerifier
//
//     val connVerifierCA = ConnectionFactory.createConnection("applicant1", "ClaimingAgent", verifierId, claimingAgentId)
//     verifierPA.store(verifierPA._dbQ, verifierPA._storeCnxn, connVerifierCA.toStoreKey, Serializer.serialize[ Data ](connVerifierCA))
//
//     val connVerifierRA = ConnectionFactory.createConnection("RA", "RelyingAgent", verifierId, relyingAgentId)
//     verifierPA.store(verifierPA._dbQ, verifierPA._storeCnxn, connVerifierRA.toStoreKey, Serializer.serialize[ Data ](connVerifierRA))
//     //Thread.sleep(TIMEOUT_LONG)
//
//     //re-run listens on verifier PA - will start listening on the connections stored above on the verifier self connection
//     verifierPA.listenForHostedCnxns
//
//     val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba", "Full", claimingAgentId, verifierId)
//     claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//     //Thread.sleep(TIMEOUT_LONG)
//
//     //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//     claimingAgentPA.listenForHostedCnxns
//
//    //store an applicant/verifier connection on the verifier
//     val verifier = Verifier( "Govt. of Manitoba")
//
//     var aliasConn = AliasConnection("applicant1", connVerifierCA.id)
//     verifierPA.store(verifierPA._dbQ, verifierPA._storeCnxn, aliasConn.toStoreKey, Serializer.serialize[ Data ](aliasConn))
//
//     val verifiedSalary = VerifiedData("applicant1", "salary", "50000", UUID.randomUUID)
//     verifierPA.store(verifierPA._dbQ, verifierPA._storeCnxn, verifiedSalary.toStoreKey, Serializer.serialize[Data](verifiedSalary))
//
//     val verifiedLastName = VerifiedData("applicant1", "lastName", "Smith", UUID.randomUUID)
//     verifierPA.store(verifierPA._dbQ, verifierPA._storeCnxn, verifiedLastName.toStoreKey, Serializer.serialize[Data](verifiedLastName))
//
//     val contentVerifierLastName = ContentVerifier( connVerifierCA.id, "BMO", "profile.lastName", "applicant1", verifier.id, true, "APPROVED")
//     verifierPA.store(verifierPA._dbQ, cnxnCAVerifier, contentVerifierLastName.toStoreKey, Serializer.serialize[Data](contentVerifierLastName))
//
//     val contentVerifierSalary = ContentVerifier( connVerifierCA.id, "BMO", "profile.salary", "applicant1", verifier.id, false, "APPROVED")
//     verifierPA.store(verifierPA._dbQ, cnxnCAVerifier, contentVerifierSalary.toStoreKey, Serializer.serialize[Data](contentVerifierSalary))
//
//     val contentVerifierBirthDate = ContentVerifier( connVerifierCA.id, "BMO", "profile.birthDate", "applicant1", verifier.id, true, "APPROVED")
//     verifierPA.store(verifierPA._dbQ, cnxnCAVerifier, contentVerifierBirthDate.toStoreKey, Serializer.serialize[Data](contentVerifierBirthDate))
//
//     var verifyResponse:VerifyResponse = null
//     var verifyPermissionRequest:VerifyPermissionRequest = null
//     var msgReceived = false
//
//     def handleVerifyPermissionRequest(cnxn:AgentCnxnProxy, msg:Message) = {
//       msg match {
//         case x:VerifyPermissionRequest => {
//           verifyPermissionRequest = x
//         }
//         case _ => {}
//       }
//       msgReceived = true
//     }
//
//     def handleVerifyResponse(cnxn:AgentCnxnProxy, msg:Message) = {
//       msg match {
//         case x:VerifyResponse => {
//           verifyResponse = x
//         }
//         case _ => {}
//       }
//       msgReceived = true
//     }
//
//     def fetchData(queue:Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ], cnxn:AgentCnxnProxy, searchKey:String):VerifyPermissionRequest = {
//       claimingAgentPA.fetch[Data](queue, cnxn, searchKey, handleFetch)
//       return verifyPermissionRequest
//     }
//
//     def handleFetch(cnxn: AgentCnxnProxy, data: Data) =
//     {
//       data match {
//         case x: PersistedRequest => {
//           x.message match {
//             case request:VerifyPermissionRequest => {
//               verifyPermissionRequest = request
//              }
//             case _ => {}
//           }
//         }
//         case _ => {}
//       }
//     }
//
////// specs error
//     "trigger a VerifyPermissionRequest message" in {
//       var msg: VerifyRequest = new VerifyRequest(new Identification(), null, "applicant1", "salary", "50000", "Applying for a loan", "Bank of Montreal")
//       msg.originCnxn = cnxnRAVerifier
//       msg.channelLevel = Some(ChannelLevel.Public)
//       relyingAgentPA.send(relyingAgentPA._msgQ, cnxnVerifierRA, msg)
//       //Thread.sleep(1000)
//
//       claimingAgentPA.listen(claimingAgentPA._msgQ, cnxnCAVerifier, Channel.Verify, ChannelType.Request, ChannelLevel.Public, handleVerifyPermissionRequest(_:AgentCnxnProxy, _:Message))
//
//       msgReceived must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyPermissionRequest.claimKey must be_==(msg.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyPermissionRequest.claimData must be_==(msg.claimData).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyPermissionRequest.reason must be_==(msg.reason).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyPermissionRequest.relyingAgentDescription must be_==(msg.relyingAgentDescription).eventually(5, TIMEOUT_EVENTUALLY)
//     }
//
//     "trigger a VerifyResponse message when content is auto-approved" in {
//       var msg: VerifyRequest = new VerifyRequest(new Identification(), null, "applicant1", "lastName", "Smith", "?", "Bank of Montreal")
//       msg.originCnxn = cnxnRAVerifier
//       msg.channelLevel = Some(ChannelLevel.Public)
//       relyingAgentPA.send(relyingAgentPA._msgQ, cnxnVerifierRA, msg)
//       //Thread.sleep(TIMEOUT_LONG)
//
//       claimingAgentPA.listenForHostedCnxns
//       //Thread.sleep(TIMEOUT_LONG)
//
//       verifierPA.listenForHostedCnxns
//       //Thread.sleep(TIMEOUT_LONG)
//
//       relyingAgentPA.listen(relyingAgentPA._msgQ, cnxnRAVerifier, Channel.Verify, ChannelType.Response, ChannelLevel.Public, handleVerifyResponse(_:AgentCnxnProxy, _:Message))
//
//       val expectedMsg = VerifyResponse(msg.ids.copyAsChild(), msg.eventKey, msg.alias, msg.claimKey, true)
//
//       msgReceived must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.ids.conversationId must be_==(expectedMsg.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.alias must be_==(expectedMsg.alias).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.claimKey must be_==(expectedMsg.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.isVerified must be_==(expectedMsg.isVerified).eventually(5, TIMEOUT_EVENTUALLY)
//     }
//
//     "trigger a persisted VerifyPermissionRequest when content is not auto-approved" in {
//       var msg: VerifyRequest = new VerifyRequest(new Identification(), null, "applicant1", "salary", "50000", "Loan application", "Bank of Montreal")
//       msg.originCnxn = cnxnRAVerifier
//       msg.channelLevel = Some(ChannelLevel.Public)
//       relyingAgentPA.send(relyingAgentPA._msgQ, cnxnVerifierRA, msg)
//       //Thread.sleep(TIMEOUT_LONG)
//
//       claimingAgentPA.listenForHostedCnxns
//       //Thread.sleep(TIMEOUT_LONG)
//
//       val expectedMsg = VerifyPermissionRequest(msg.ids.copyAsChild(), null, msg.claimKey, msg.claimData, msg.reason, msg.relyingAgentDescription)
//
//       //Thread.sleep(TIMEOUT_LONG)
//       val search = new Search[PersistedRequest](classOf[PersistedRequest])
//       search.setSearchFieldValue("messageType", "VerifyPermissionRequest")
//       verifyPermissionRequest = fetchData(claimingAgentPA._dbQ, cnxnCAVerifier, search.toSearchKey)
//       //Thread.sleep(TIMEOUT_LONG)
//
//       expectedMsg.ids.conversationId must be_==(verifyPermissionRequest.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//       expectedMsg.claimKey must be_==(verifyPermissionRequest.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//       expectedMsg.claimData must be_==(verifyPermissionRequest.claimData).eventually(5, TIMEOUT_EVENTUALLY)
//       expectedMsg.reason must be_==(verifyPermissionRequest.reason).eventually(5, TIMEOUT_EVENTUALLY)
//       expectedMsg.relyingAgentDescription must be_==(verifyPermissionRequest.relyingAgentDescription).eventually(5, TIMEOUT_EVENTUALLY)
//     }
//
//     "trigger a verify response of false when verified data does not match claim" in {
//       var msg: VerifyRequest = new VerifyRequest(new Identification(), null, "applicant1", "lastName", "Jones", "?", "Bank of Montreal")
//       msg.originCnxn = cnxnRAVerifier
//       msg.channelLevel = Some(ChannelLevel.Public)
//       relyingAgentPA.send(relyingAgentPA._msgQ, cnxnVerifierRA, msg)
//       //Thread.sleep(TIMEOUT_LONG)
//
//       claimingAgentPA.listenForHostedCnxns
//       //Thread.sleep(TIMEOUT_LONG)
//
//       verifierPA.listenForHostedCnxns
//       //Thread.sleep(TIMEOUT_LONG)
//
//       relyingAgentPA.listen(relyingAgentPA._msgQ, cnxnRAVerifier, Channel.Verify, ChannelType.Response, ChannelLevel.Public, handleVerifyResponse(_:AgentCnxnProxy, _:Message))
//
//       val expectedMsg = VerifyResponse(msg.ids.copyAsChild(), msg.eventKey, msg.alias, msg.claimKey, false)
//
//       msgReceived must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.ids.conversationId must be_==(expectedMsg.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.alias must be_==(expectedMsg.alias).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.claimKey must be_==(expectedMsg.claimKey).eventually(5, TIMEOUT_EVENTUALLY)
//       verifyResponse.isVerified must be_==(expectedMsg.isVerified).eventually(5, TIMEOUT_EVENTUALLY)
//     }
//   }
//
//  "Sending a SetContentPersistedRequest for a persisted VerifyPermissionRequest message" should {
//    //skip("don't run unless all three rabbits are running")
//
//    val agentSessionId = UUID.randomUUID()
//
//    val relyingAgentId = "RelyingAgent" + UUID.randomUUID.toString
//    val claimingAgentId = "ClaimingAgent" + UUID.randomUUID.toString
//    val verifierId = "Verifier" + UUID.randomUUID.toString
//
//    val cnxnCASelf = AgentCnxnProxy(claimingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnRASelf = AgentCnxnProxy(relyingAgentId.toURI, "", relyingAgentId.toURI)
//    val cnxnRACA = AgentCnxnProxy(relyingAgentId.toURI, "", claimingAgentId.toURI)
//    val cnxnVerifierCA = AgentCnxnProxy(verifierId.toURI, "", claimingAgentId.toURI)
//    val cnxnCAVerifier= AgentCnxnProxy(claimingAgentId.toURI, "", verifierId.toURI)
//
//    val relyingAgentPA = createRelyingAgent(List(cnxnRASelf,cnxnCASelf))
//    val claimingAgentPA = createClaimingAgent(List(cnxnRASelf, cnxnCASelf))
//    val verifierPA = createVerifier
//
//    //store a CA/RA connection on the CA self connection
//    val connCARA = ConnectionFactory.createConnection("RA", "Full", claimingAgentId, relyingAgentId)
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCARA.toStoreKey, Serializer.serialize[ Data ](connCARA))
//
//    //store a CA/RA connection on the CA self connection
//    val connCAVerifier = ConnectionFactory.createConnection("Govt. of Manitoba", "Full", claimingAgentId, verifierId, false, List("verifier"))
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCASelf, connCAVerifier.toStoreKey, Serializer.serialize[ Data ](connCAVerifier))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on claiming agent PA - will start listening on the connections stored above on the claiming agent pa self connection
//    claimingAgentPA.listenForHostedCnxns
//
//    //store a verifier/CA connection on the Verifier self connection
//    var connRACA = ConnectionFactory.createConnection("CA", "Full", relyingAgentId, claimingAgentId)
//    relyingAgentPA.store(relyingAgentPA._dbQ, cnxnRASelf, connRACA.toStoreKey, Serializer.serialize[ Data ](connRACA))
//    //Thread.sleep(TIMEOUT_LONG)
//
//    //re-run listens on relying agent PA - will start listening on the connections stored above on the relying agent pa self connection
//    relyingAgentPA.listenForHostedCnxns
//
//    val verifier = Verifier( "Government of Manitoba")
//    val verifiers = List(verifier)
//
//    val profile = Profile( "John", "Smith", "12345")
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnRACA, profile.toStoreKey, Serializer.serialize[Data](profile))
//
//    val contentVerifier = ContentVerifier( connCAVerifier.id, "GoM", "profile.lastName", "applicant1", verifier.id, true, "APPROVED")
//    claimingAgentPA.store(claimingAgentPA._dbQ, cnxnRACA, contentVerifier.toStoreKey, Serializer.serialize[Data](contentVerifier))
//
//    var verifyPermissionResponse:VerifyPermissionResponse = null
//
//     def handleVerifyPermissionResponse(cnxn:AgentCnxnProxy, msg:Message) = {
//       msg match {
//         case x:VerifyPermissionResponse => {
//           verifyPermissionResponse = x
//         }
//         case _ => {}
//       }
//     }
//
//    "trigger a VerifyPermissionResponse message" in {
//      var request = VerifyPermissionRequest(new Identification(), new EventKey(agentSessionId, "verifyPermissionRequest"), "salary", "50000", "Loan application", "Bank of Montreal")
//      request.targetCnxn = cnxnCAVerifier
//      request.originCnxn = cnxnVerifierCA
//      request.channelLevel = Some(ChannelLevel.Public)
//      val persistedRequest = PersistedRequest( "VerifyPermissionRequest", request, new DateTime())
//      claimingAgentPA.store(claimingAgentPA._dbQ, cnxnCAVerifier, persistedRequest.toStoreKey, Serializer.serialize[Data](persistedRequest))
//      //Thread.sleep(TIMEOUT_LONG)
//
//      val conn = ConnectionFactory.createConnection("CAVerifier", "Full", claimingAgentId, verifierId)
//      val compositeData = new CompositeData[PersistedRequest](conn, persistedRequest)
//      val msg = SetContentPersistedRequest(new EventKey(agentSessionId, "verifierSelected"), compositeData, null, true)
//      msg.targetCnxn = cnxnCAVerifier
//      msg.originCnxn = claimingAgentPA._cnxnUIStore
//      msg.channelLevel = Some(ChannelLevel.Public)
//      claimingAgentPA.send(claimingAgentPA._msgQ, cnxnCAVerifier, msg)
//      //Thread.sleep(TIMEOUT_LONG)
//
//      verifierPA.listen(verifierPA._msgQ, cnxnVerifierCA, Channel.Verify, ChannelType.Response, ChannelLevel.Public, handleVerifyPermissionResponse(_:AgentCnxnProxy, _:Message))
//      //Thread.sleep(TIMEOUT_LONG)
//
//      val expectedMsg = VerifyPermissionResponse(request.ids.copyAsChild, true)
//
//      expectedMsg.ids.conversationId must be_==(verifyPermissionResponse.ids.conversationId).eventually(5, TIMEOUT_EVENTUALLY)
//      expectedMsg.isPermissionGranted must be_==(verifyPermissionResponse.isPermissionGranted).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//  }
//}
