package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.util._
import java.util.UUID

trait IntroductionRequestSetCreator
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicIntroductionCreatorRequests(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Introduction, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Public, handlePublicIntroductionCreatorRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handlePublicIntroductionCreatorRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicIntroductionCreatorRequestChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: CreateIntroductionRequest => {
        //TODO: Not sure if this processing is correct
        processCreateIntroductionRequest(cnxn, x)
      }

      case _ => report("***********************not doing anything in handlePublicIntroductionCreatorRequestChannel", Severity.Error)
    }
    report("exiting handlePublicIntroductionCreatorRequestChannel in ConnectionBroker", Severity.Trace)
  }

  protected def processCreateIntroductionRequest(cnxnBroker_A: AgentCnxnProxy, createIntroductionsRequest: CreateIntroductionRequest) =
  {
    //agent wants available introductions from the broker cnxn, intros are one-sided
    //need to get to broker self cnxn, then lookup introduction profiles/packages
    report("****CREATE INTRODUCTIONS REQUEST RECEIVED:****", Severity.Debug)
    val queryObject = SystemDataFactory.createEmptyImmutableSystemDataForConnectionSearch()

    fetch[ SystemData[ Connection ] ](_dbQ, cnxnBroker_A, queryObject.toSearchKey, handleSystemDataLookupCreateIntroductions(_: AgentCnxnProxy, _: SystemData[ Connection ], cnxnBroker_A, createIntroductionsRequest))
  }

  protected def handleSystemDataLookupCreateIntroductions(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], cnxnBroker_A: AgentCnxnProxy, createIntroductionsRequest: CreateIntroductionRequest): Unit =
  {
    //find all connections the broker has
    val queryObject = ConnectionFactory.createEmptyImmutableConnectionForSearch()
    fetchList[ Connection ](_dbQ, systemConnection.data.writeCnxn, queryObject.toSearchKey, generateIntroductions(_: AgentCnxnProxy, _: List[ Connection ], cnxnBroker_A, createIntroductionsRequest))
  }

  protected def generateIntroductions(cnxnBrokerSelf: AgentCnxnProxy, connsBroker: List[ Connection ], cnxnBroker_A: AgentCnxnProxy, createIntroductionsRequest: CreateIntroductionRequest) =
  {
    report("****GENERATE INTRODUCTIONS REQUEST:****", Severity.Debug)
    val findConnBroker_A = connsBroker.filter(x => x.writeCnxn == cnxnBroker_A)
    findConnBroker_A.headOption match {
      case None => report("cannot find connBroker_A", Severity.Error)
      case Some(connBroker_A) => {
        for ( connBroker_B <- connsBroker.filter(x => x.readCnxn != connBroker_A.readCnxn && x.writeCnxn != connBroker_A.writeCnxn) ) {
          findIntroductionProfile(connBroker_A, connBroker_B)
        }
      }
    }

    //notify user that introductions have been sent
    val response = new CreateIntroductionResponse(createIntroductionsRequest.ids.copyAsChild(), createIntroductionsRequest.eventKey, "success")
    response.targetCnxn = createIntroductionsRequest.targetCnxn
    response.originCnxn = createIntroductionsRequest.originCnxn

    report("****Post save introductions, sending the response: " + response.getChannelKey + "****", Severity.Info)
    send(_publicQ, response.targetCnxn, response)

    //todo:send a notification to the user if he/she is logged in
  }

  protected def findIntroductionProfile(connBroker_A: Connection, connBroker_B: Connection): Unit =
  {
    //find all introduction profiles that are shared - should only be 1
    val queryObject = new IntroductionProfile ()
    fetchList[ IntroductionProfile ](_dbQ, connBroker_B.readCnxn, queryObject.toSearchKey, generateIntroduction(_: AgentCnxnProxy, _: List[ IntroductionProfile ], connBroker_A, connBroker_B))
  }

  protected def generateIntroduction(cnxnB_Broker: AgentCnxnProxy, introductionProfiles: List[ IntroductionProfile ], connBroker_A: Connection, connBroker_B: Connection) =
  {
    introductionProfiles.headOption match {
      case None => report("cannot generate introduction, no introduction profile is shared ", Severity.Debug)
      case Some(introductionProfile) => {
        val intro = new Introduction(introductionProfile.alias, connBroker_A.writeCnxn)

        //we can be confident that any broker_... connections are persisted on this pa
        //obsolete-intro state should exist before CreateInvitationRequest based on Introduction so we process it first
//        report("Saving Introduction State for " + connBroker_B.alias + " to " + connBroker_A.writeCnxn.toString, Severity.Trace)
//        updateDataById(connBroker_A.writeCnxn, introState, null)
        report("Saving Introduction for " + connBroker_B.alias + " to " + connBroker_A.writeCnxn.toString, Severity.Debug)
        updateDataById(connBroker_A.writeCnxn, intro, null)
      }
    }
  }
}