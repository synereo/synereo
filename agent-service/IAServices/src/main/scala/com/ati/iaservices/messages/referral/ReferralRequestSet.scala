package com.ati.iaservices.messages.referral

import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util.Severity
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.util._
import com.ati.iaservices.platformagents.AgentHostStorePlatformAgent

trait ReferralRequestSet
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicReferralRequests(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Referral, ChannelType.Request, ChannelLevel.Public, handlePublicReferralRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handlePublicReferralRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicReferralRequestChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: CreateReferralRequest => {
        processCreateReferralRequest(cnxn, x)
      }

      case _ => report("***********************not doing anything in handlePublicReferralRequestChannel", Severity.Error)
    }
    report("exiting handlePublicReferralRequestChannel in ConnectionBroker", Severity.Trace)
  }

  protected def processCreateReferralRequest(cnxnBroker: AgentCnxnProxy, createReferralRequest: CreateReferralRequest) =
  {
    //agent likes the introduction/search result and wants to invite the other agent to establish a connection
    //as broker you control this initial invite protocol
    //generate an invitation to both agents
    report("---->createReferralRequest:" + createReferralRequest + ", cnxnBroker=" + cnxnBroker + "createReferralRequest PARENT=" + createReferralRequest.ids.parentId)
//    report("****CREATE REFERRAL REQUEST RECEIVED:****", Severity.Debug)
    //we need to do a lookup of the targetConnections by id in Broker Self Connection to find all the agents to invite
    createReferralRequest.deliver();

    val query = SystemDataFactory.SEARCH_ALL_CONNECTION
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnBroker, query.toSearchKey, handleSystemDataLookupCreateReferral(_: AgentCnxnProxy, _: SystemData[ Connection ], createReferralRequest))
  }

  protected def handleSystemDataLookupCreateReferral(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], createReferralRequest: CreateReferralRequest): Unit =
  {
    //TODO: inefficient but best we can do until toSearchKey can handle lookup by AgentCnxnProxy
    val query = Connection.SEARCH_ALL
    //comes in as write=BrokerSelf, read=RandomSelf, we want BrokerSelf
    fetchList[ Connection ](_dbQ, systemConnection.data.writeCnxn, query.toSearchKey, findConnections(_: AgentCnxnProxy, _: List[ Connection ], createReferralRequest))
  }

  // search for connections Broker_A & broker_B
  protected def findConnections(cnxnBroker_Broker: AgentCnxnProxy, connsBroker: List[ Connection ], createReferralRequest: CreateReferralRequest) =
  {
    report("findConnections " + connsBroker)
    val findConnBroker_A = connsBroker.filter(x => x.writeCnxn.getExchangeKey() == createReferralRequest.invitationConnectionId_A)
    findConnBroker_A.headOption match {

      case None => {
        report("cannot find connBroker_A", Severity.Error)
      }
      case Some(connBroker_A) => {
        val findConnBroker_B = connsBroker.filter(x => x.writeCnxn.getExchangeKey() == createReferralRequest.invitationConnectionId_B)
        findConnBroker_B.headOption match {
          case None => {
            report("cannot find connBroker_B", Severity.Error)
          }
          case Some(connBroker_B) => {
            report("generateInvitationReferralRequests --->")
            generateInvitationRequests(createReferralRequest, connBroker_A, connBroker_B)
          }
        }
      }
    }
  }

  def generateInvitationRequests(sourceRequest: CreateReferralRequest, connBroker_A: Connection, connBroker_B: Connection) : Unit =
  {
    //we shouldn't be using our own alias for referrals, should use whats publicly shared with us, pass it in instead?
    //the user will have to pick the category
    generateInvitationRequests(
      sourceRequest,
      false,
      connBroker_A,
      connBroker_B,
      connBroker_A.alias,
      connBroker_B.alias,
      connBroker_A.category,
      connBroker_B.category,
      None,
      None,
      None,
      None,
      getPosts(sourceRequest.post_A),
      getPosts(sourceRequest.post_B),
      handleSendInvitationResponse(_: Message with Request, _: String)
    )
  }

//  protected def getPosts(post: Post) : List[ Post ] =
//  {
//    if ( post == null )
//      Nil
//    else
//      post :: Nil
//  }

  protected def handleSendInvitationResponse(sourceRequest: Message with Request, status: String): Unit =
  {
    //notify user that both invites have been sent
    val response = new CreateReferralResponse(sourceRequest.ids.copyAsChild(), sourceRequest.eventKey, "success")
    response.targetCnxn = sourceRequest.targetCnxn
    response.originCnxn = sourceRequest.originCnxn

    report("****Post save invite, sending the response: " + response.getChannelKey + "****", Severity.Info)
    send(_publicQ, response.targetCnxn, response)

  }

  //  protected def deleteReferral(cnxnBroker_A: AgentCnxnProxy, introductionId: String) =
  //  {
  //    if ( introductionId != "" ) {
  //      val query = new Introduction()
  //      query.id = introductionId.toString
  //
  //      deleteDataBySearch(cnxnBroker_A, query, None)
  //    }
  //  }

}