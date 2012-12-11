package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
import content.{SetContentRequest, SetSelfContentRequest}
import scala.util.continuations._
import scala.concurrent.ops._
import com.protegra_ati.agentservices.core.schema.util._
import java.util.UUID
import java.util.HashMap
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.protegra_ati.agentservices.core.util.ThreadRenamer._
import com.protegra_ati.agentservices.core.util.cloner.ClonerFactory


trait InvitationRequestSetCreator
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicInvitationCreatorRequests(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Public, handlePublicInvitationCreatorRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handlePublicInvitationCreatorRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicInvitationCreatorRequestChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: CreateInvitationRequest => {
        processCreateInvitationRequest(cnxn, x)
      }

      case x: ReferralRequest => {
        processReferralRequest(cnxn, x)
      }

      case _ => report("***********************not doing anything in handlePublicInvitationCreatorRequestChannel", Severity.Error)
    }
    report("exiting handlePublicInvitationCreatorRequestChannel in ConnectionBroker", Severity.Trace)
  }

  protected def processCreateInvitationRequest(cnxnBroker_A: AgentCnxnProxy, createInviteRequest: CreateInvitationRequest) =
  {
    //agent likes the introduction/search result and wants to invite the other agent to establish a connection
    //as broker you control this initial invite protocol
    //generate an invitation to both agents
    // here has to be changed
    report("-!!!!!!!!!!!!!!!!!--->createInviteRequest:" + createInviteRequest + ", cnxnBroker_A=" + cnxnBroker_A + "createInviteRequest PARENT=" + createInviteRequest.ids.parentId)
    report("****CREATE INVITATION REQUEST RECEIVED:****", Severity.Debug)
    createInviteRequest.deliver();

    //we need to do a lookup of the targetConnection by id in Broker Self Connection to find the agent to invite
    val query = SystemDataFactory.createEmptyImmutableSystemDataForConnectionSearch()
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnBroker_A, query.toSearchKey, handleSystemDataLookupCreateReferral(_: AgentCnxnProxy, _: SystemData[ Connection ], createInviteRequest, cnxnBroker_A))
  }

  protected def handleSystemDataLookupCreateReferral(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], createInviteRequest: CreateInvitationRequest, cnxnBroker_A: AgentCnxnProxy): Unit =
  {
    //TODO: innefficient but best we can do until toSearchKey can handle lookup by AgentCnxnProxy
    //comes in as write=BrokerSelf, read=RandomSelf, we want BrokerSelf
    generateReferralRequest(createInviteRequest, systemConnection.data)
  }

  protected def generateReferralRequest(
    sourceRequest: CreateInvitationRequest,
    connBrokerBroker: Connection) =
  {
    report("****GENERATE Referral REQUEST:****")
    report("****Found valid self connection, sending referral for cnxn A: " + sourceRequest.selfAlias + " to cnxn B: " + sourceRequest.targetAlias, Severity.Debug)

    //wait on broker permission (sends a message to the brokerSelf to be persisted there)
    sendReferralRequest(sourceRequest, connBrokerBroker)
    handleSendResponse(sourceRequest, "success")
    //todo:send a notification to the user
  }

  protected def sendReferralRequest(sourceRequest: CreateInvitationRequest, conn: Connection): Unit =
  {
    val req = new ReferralRequest(sourceRequest.ids.copyAsChild(), sourceRequest.eventKey, sourceRequest)
    req.targetCnxn = conn.writeCnxn
    req.originCnxn = conn.writeCnxn
    report("req=" + req + ", target=" + req.targetCnxn + ", origin=" + req.originCnxn)
    send(_publicQ, conn.writeCnxn, req)
  }

  def createFromDataForReferral(): HashMap[ String, Data ] =
  {
      val map = new HashMap[String, Data]
      var tempProfile = new Profile( )  // STRESS TODO eventually it worse to create a singleton like Profile.SEARCH_ALL
      tempProfile.firstName = "App"

      map.put( tempProfile.formattedClassName, tempProfile )
      return map
  }

  protected def processReferralRequest(cnxnSelf: AgentCnxnProxy, referralRequest: ReferralRequest) =
  {
    report("STORE REFERRAL FOR LATER RESPONSE: referralRequest=" + referralRequest + ", cnxn=" + cnxnSelf, Severity.Info)
    // persists ReferralRequest for the broker
    val persistedMessage = new PersistedMessage[ ReferralRequest ](referralRequest)

    report("attempting to store " + persistedMessage.toStoreKey)
    store(_dbQ, cnxnSelf, persistedMessage.toStoreKey, Serializer.serialize[ PersistedMessage[ ReferralRequest ] ](persistedMessage))

    if ( cnxnSelf.src.toString.contains(BIZNETWORK_AGENT_ID) ) {
      val subject = ""
      val body = "Automatically accepting referral for owner"
      val fromDetails = createFromDataForReferral()
      val postToTarget = new Post(subject, body, fromDetails)
      val response = new ReferralResponse(referralRequest.ids.copyAsChild(), referralRequest.eventKey, referralRequest.source, postToTarget, null, true)
      response.targetCnxn = referralRequest.targetCnxn
      response.originCnxn = referralRequest.originCnxn
      send(_publicQ, cnxnSelf, response )
    }
    referralRequestNotificationHandler(cnxnSelf, referralRequest)

  }
  protected def referralRequestNotificationHandler(cnxn: AgentCnxnProxy, referralRequest: ReferralRequest) =
  {
    //email, sms, however you want to notify
    //also a hook to override in other implementations
  }

  def generateInvitationRequests(sourceRequest: CreateInvitationRequest, connBroker_A: Connection, connBroker_B: Connection): Unit =
  {
    generateInvitationRequests(
      sourceRequest,
      connBroker_A,
      connBroker_B,
      null, null
    )
  }

  /**
   * creates a connection rejecting post
   * @param eventKey  event key
   * @param targetToBrokerConnection connection from ta target to the broker
   * @param targetToBroker  post from the target to the broker
   */
  def generateRejectToInvitationRequest(eventKey: EventKey, targetToBrokerConnection: Connection, targetToBroker: Post): Unit =
  {
    val queryObject = SystemDataFactory.createEmptyImmutableSystemDataForConnectionSearch()
    fetch[ SystemData[ Connection ] ](_dbQ, targetToBrokerConnection.readCnxn, queryObject.toSearchKey, findSelfConToSendPost(_: AgentCnxnProxy, _: SystemData[ Connection ], eventKey, targetToBrokerConnection, targetToBroker))
  }

  /**
   *  lookup to find a self connection to send rejecting post to the broker and store in a self connection
   * @param cnxn target - brocker cnxn
   * @param systemConnection  containing a self connection
   * @param eventKey event key
   * @param targetToBrokerConnection target- broker connection
   * @param targetToBroker post to be sent
   */
  protected def findSelfConToSendPost(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], eventKey: EventKey, targetToBrokerConnection: Connection, targetToBroker: Post): Unit =
  {
    val req = SetContentRequest(eventKey, new CompositeData[ Post ](targetToBrokerConnection, targetToBroker), null)
    req.targetCnxn = systemConnection.data.writeCnxn
    req.originCnxn = systemConnection.data.readCnxn
    processSetContentRequest(req)
  }


  // TODO description
  def generateInvitationRequests(sourceRequest: CreateInvitationRequest, connBroker_A: Connection, connBroker_B: Connection, refereeToTarget: Post, refereeToSource: Post): Unit =
  {
    generateInvitationRequests(
      sourceRequest,
      connBroker_A,
      connBroker_B,
      sourceRequest.selfAlias,
      sourceRequest.targetAlias,
      sourceRequest.selfCategory,
      sourceRequest.targetCategory,
      Some(sourceRequest.requestedConnectionType),
      Some(sourceRequest.requestedConnectionType),
      Some(sourceRequest.requestedConnectionName),
      Some(sourceRequest.requestedConnectionName),
      getPosts(sourceRequest.postToBroker) ::: getPosts(refereeToSource),
      getPosts(sourceRequest.postToTarget) ::: getPosts(refereeToTarget),
      handleSendResponse(_: Message with Request, _: String)
    )
  }

  protected def getPosts(post: Post): List[ Post ] =
  {
    if ( post == null )
      Nil
    else
      post :: Nil
  }

  /**
   * Two invitation requests will be created and send to both conversation parties
   * @param sourceRequest
   * @param connBroker_A
   * @param connBroker_B
   * @param alias_A
   * @param alias_B
   * @param category_A
   * @param category_B
   * @param requestedConnectionType_A
   * @param requestedConnectionType_B
   * @param requestedConnectionName_A
   * @param requestedConnectionName_B
   * @param requestedPosts_A  post thread forwarded to A
   * @param requestedPosts_B  post thread forwarded to B
   * @param sendResponseHandler
   */
  def generateInvitationRequests(
    sourceRequest: Message with Request,
    connBroker_A: Connection,
    connBroker_B: Connection,
    alias_A: String,
    alias_B: String,
    category_A: String,
    category_B: String,
    requestedConnectionType_A: Option[ String ],
    requestedConnectionType_B: Option[ String ],
    requestedConnectionName_A: Option[ String ],
    requestedConnectionName_B: Option[ String ],
    requestedPosts_A: List[ Post ],
    requestedPosts_B: List[ Post ],
    sendResponseHandler: (Message with Request, String) => Unit) =
  {
    report("****GENERATE INVITATION REQUEST:****")
    report("****Found valid target connection, sending invites to cnxn A: " + connBroker_A.readCnxn.toString + " and cnxn B: " + connBroker_B.readCnxn.toString, Severity.Debug)

    //invite both parties with inverse
    val inviteA = sendInvitationRequest(sourceRequest, connBroker_A, alias_B, Some(category_B), requestedConnectionType_A, requestedConnectionName_A, requestedPosts_A)
    val inviteB = sendInvitationRequest(sourceRequest, connBroker_B, alias_A, Some(category_A), requestedConnectionType_B, requestedConnectionName_B, requestedPosts_B)
    waitForInvitationResponse(connBroker_A.writeCnxn, inviteA, inviteB)
    // signal successful sending
    sendResponseHandler(sourceRequest, "success")
    //todo:send a notification to the user
  }

  protected def handleSendResponse(sourceRequest: Message with Request, status: String): Unit =
  {
    //notify user that both invites have been sent
    val response = new CreateInvitationResponse(sourceRequest.ids.copyAsChild(), sourceRequest.eventKey, status)
    response.targetCnxn = sourceRequest.targetCnxn
    response.originCnxn = sourceRequest.originCnxn

    report("****Post save invite, sending the response: " + response.getChannelKey + "****", Severity.Info)
    send(_publicQ, response.targetCnxn, response)
  }


  protected def sendInvitationRequest(sourceRequest: Message with Request,
    conn: Connection,
    alias: String,
    category: Option[ String ],
    requestedConnectionType: Option[ String ],
    requestedConnectionName: Option[ String ],
    requestedPosts: List[ Post ]
    ): InvitationRequest =
  {
    val req = new InvitationRequest(sourceRequest.ids.copyAsChild(), sourceRequest.eventKey, alias, category, requestedConnectionType, requestedConnectionName, requestedPosts)
    req.targetCnxn = conn.readCnxn
    req.originCnxn = conn.readCnxn
    report("req=" + req + ", target=" + req.targetCnxn + ", origin=" + req.originCnxn)
    send(_publicQ, req.targetCnxn, req)
    req
  }

  protected def waitForInvitationResponse(cnxnBroker_A: AgentCnxnProxy, inviteA: InvitationRequest, inviteB: InvitationRequest) =
  {
    //we need to start listening for the one time InvitationResponse messages from both sides
    report("single listen: channel: " + inviteA.getResponseChannelKey + " cnxn: " + inviteA.originCnxn, Severity.Info)

//    report("single listen: channel: " + inviteA.getResponseChannelKey.toLabel + " cnxn: " + inviteA.originCnxn)


    //    listen(_publicQ, invite1.originCnxn, invite1.getResponseChannelKey, handleFirstResponseReceived(_: AgentCnxnProxy,  _: Message, invite2))

    val agentCnxnA = inviteA.originCnxn.toAgentCnxn()
    reset {
      for ( e <- _publicQ.subscribe(agentCnxnA)(inviteA.getResponseChannelKey.toLabel) ) {
        //TODO: temporary hack until multiple get can be used. right now we get e & none or none & f
        if ( e != None ) {

          Thread.sleep(500)
          // val msgA = Serializer.deserialize[ InvitationResponse ](syncE.dispatch)
//          report("!!! Listen Received FOR FIRST InvitationResponse !!!" + inviteA + ", e=" + e)

          report("!!! Listen Received FOR FIRST InvitationResponse !!!: ", Severity.Debug)
          //TODO: see if this is really necessary or only a unit test issue, may go away once "multiple get for" is in place
          val agentCnxnB = inviteB.originCnxn.toAgentCnxn()

          reset {
            for ( f <- _publicQ.subscribe(agentCnxnB)(inviteB.getResponseChannelKey.toLabel) ) {
              if ( e != None && f != None ) {
                report("!!! Listen Received FOR BOTH InvitationResponse MESSAGES !!!: ", Severity.Debug)

                spawn {
                  rename {
                  val msgA = Serializer.deserialize[ InvitationResponse ](e.dispatch)
                  val msgB = Serializer.deserialize[ InvitationResponse ](f.dispatch)
                    if ( !msgB.accept ) {
                    // target to broker con
                    //find Invite self using system data
                    val inviteB2Broker = ConnectionFactory.createTempConnection(msgB.connectionName, msgB.connectionType, inviteB.originCnxn, inviteB.targetCnxn);
                    generateRejectToInvitationRequest(inviteB.eventKey, inviteB2Broker, msgB.getPost())
                  }
                  else if ( msgA.accept && msgB.accept ) {
                    report("****GENERATE CREATE CONNECTION REQUEST:****", Severity.Info)
                    val aId = UUID.randomUUID()
                    val bId = UUID.randomUUID()
                    val connAB = ConnectionFactory.createConnection(msgA.connectionName, msgB.category, msgA.category, msgA.connectionType, aId.toString, bId.toString);
                    val connBA = ConnectionFactory.createConnection(msgB.connectionName, msgA.category, msgB.category, msgB.connectionType, bId.toString, aId.toString);

                    sendCreateConnectionRequest(msgA, connAB)
                    sendCreateConnectionRequest(msgB, connBA)
                    //handleMutualConnectionAgreement(msgA, msgB)

                  }
                  else {
                    report("At least one invite declined -- A: " + msgA.accept + " B: " + msgB.accept, Severity.Error)
                  }

                  try {
                    processInvitationResponseToArchive(inviteA.originCnxn, msgA)
                    processInvitationResponseToArchive(inviteB.originCnxn, msgB)
                  }
                  catch {
                    case e: Exception => e.printStackTrace()
                  }
                  //enhance later but for now just delete both invitations as both have replied and we have started the processing chain or one (or both) has declined...
                  //move this to a passed in post processing handler
                  //                  deleteIntroduction(cnxnBroker_A, introductionId)
                  //                  deleteIntroduction(cnxnBroker_A, introductionId)
                  //                deleteIntroductionState(cnxnBroker_A, introductionState.introductionId)
                  }("waits for invitation responses and create connections")
                }
              }
            }
          }
        }
      }
    }
  }

  protected def processInvitationResponseToArchive(cnxnA_Broker: AgentCnxnProxy, invitationResponse: InvitationResponse) =
  {
    //get self cnxn from system data
    //lookup the self connection from the systemdata in the connection silo
    val queryObject = SystemDataFactory.createEmptyImmutableSystemDataForConnectionSearch()
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnA_Broker, queryObject.toSearchKey, findInvitationResponseToArchive(_: AgentCnxnProxy, _: SystemData[ Connection ], invitationResponse))

    //CLEAN THIS UP  CF
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnA_Broker, queryObject.toSearchKey, findCreateInvitationRequestToArchive(_: AgentCnxnProxy, _: SystemData[ Connection ], invitationResponse))
  }

  protected def findInvitationResponseToArchive(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], invitationResponse: InvitationResponse): Unit =
  {
    val query = new PersistedMessage[ InvitationResponse ]()
    fetchList[ PersistedMessage[ InvitationRequest ] ](_dbQ, systemConnection.data.writeCnxn, query.toSearchKey, archivePersistedMessage(_: AgentCnxnProxy, _: List[ PersistedMessage[ InvitationRequest ] ], invitationResponse.ids.parentId, invitationResponse.accept))
  }

  protected def findCreateInvitationRequestToArchive(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], invitationResponse: InvitationResponse): Unit =
  {
    val query = new PersistedMessage[ CreateInvitationRequest ]()
    fetchList[ PersistedMessage[ CreateInvitationRequest ] ](_dbQ, systemConnection.data.writeCnxn, query.toSearchKey, myArchivePersistedMessage(_: AgentCnxnProxy, _: List[ PersistedMessage[ CreateInvitationRequest ] ], invitationResponse.ids.conversationId, invitationResponse.accept))
  }

  //copied from existing archivePersistedMessage
  //as it compares message sbased on parentId and Id
  //CIR's can only be matched with IRs via ConversationId
  def myArchivePersistedMessage(cnxnBroker_Broker: AgentCnxnProxy, messages: List[ PersistedMessage[ _ <: Message ] ], conversationId: String, isAccepted: Boolean) =
  {
    //TODO: fix toSearchKey to work with the nested id, once fixed just send a SetContentRequest to self
    for ( msg <- messages ) {
      if ( msg.message.ids.conversationId == conversationId ) {
        val newData = ClonerFactory.getInstance().createDeepClone(msg)
        newData.archive()
        if ( !isAccepted )
          newData.reject()
        //TODO: something is not right, its not safedeleting the old, getting 2 copies!
        updateDataById(cnxnBroker_Broker, newData, msg)
      }
    }
  }

  protected def sendCreateConnectionRequest(inviteResponse: InvitationResponse, conn: Connection) =
  {

    val req = new SetSelfContentRequest(inviteResponse.ids.copyAsChild(), inviteResponse.eventKey, conn, null)
    req.targetCnxn = inviteResponse.originCnxn
    req.originCnxn = inviteResponse.originCnxn
    send(_publicQ, inviteResponse.originCnxn, req)
  }
  protected def  handleMutualConnectionAgreement(msgA: InvitationResponse, msgB: InvitationResponse)
  {

    //send email sms etc.....to be overridden in above levels.
  }
  //  protected def deleteIntroduction(cnxnBroker_A: AgentCnxnProxy, introductionId: String) =
  //  {
  //    if ( introductionId != "" ) {
  //      val query = new Introduction()
  //      query.id = introductionId.toString
  //
  //      deleteDataBySearch(cnxnBroker_A, query, None)
  //    }
  //  }

  //
  //  protected def deleteIntroductionState(cnxnBroker_A: AgentCnxnProxy, introductionId: String) =
  //  {
  //    val introductionStateSearch = new IntroductionState (introductionId, null, null)
  //
  //    deleteDataBySearch(cnxnBroker_A, introductionStateSearch, None)
  //  }

}