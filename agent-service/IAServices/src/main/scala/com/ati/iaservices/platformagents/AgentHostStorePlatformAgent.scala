package com.ati.iaservices.platformagents

/*
 * This is meant to be used as a go between for the Host UI to DB, Host UI to Public for certain things like Gets
 */

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.util.Severity

import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent => CoreAgentHostStorePlatformAgent}
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.invitation.CreateInvitationRequest
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.ati.iaservices.messages.referral.{ReferralRequestSetPrivate, ReferralResponseSet, ReferralRequestSet}
import scala.Some

class AgentHostStorePlatformAgent() extends CoreAgentHostStorePlatformAgent()
//with InvitationNotifications
with ReferralRequestSet
with ReferralResponseSet
with ReferralRequestSetPrivate
{
  override def init(configMgr: ConfigurationManager)
  {
    super.init(configMgr)
  }

  override def loadQueues()
  {
    super.loadQueues()
  }


  override protected def listenForUICnxns() =
  {
    //ui store
    listenPrivateReferralRequest(_cnxnUIStore)
    super.listenForUICnxns()
  }

  override def listenPublicRequests(cnxn: AgentCnxnProxy)
  {
    listenPublicReferralRequests(cnxn)
    super.listenPublicRequests(cnxn)
  }

  override def listenPublicResponses(cnxn: AgentCnxnProxy)
  {
    listenPublicReferralResponses(cnxn)
    super.listenPublicResponses(cnxn)
  }

  //TODO: better way of match so we don't have duplicate logic?
  override def send(queue: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse], cnxn: AgentCnxnProxy, msg: Message)
  {
    msg match {
      case x: CreateInvitationRequest => {
        capture(cnxn, x)
      }
      case _ => {/*ignore*/}
    }

    if (msg.channelLevel == Some(ChannelLevel.Public) && isLocalNetworkMode) {
      spawn{
        processPublicSendLocally(cnxn, msg)
      }
    }
    else {
      super.send(queue, cnxn, msg)
    }
  }

  /**
   * To improve performance, when a single store PA is being used, no reason to send requests out onto public q.
   * The send method (above) intercepts messages that would normally be sent on public queue, and instead sends them to this method
   * which directs the message directly to the local handler, thus avoiding a round trip to the public q.
   */
  override protected def processPublicSendLocally(cnxn: AgentCnxnProxy, msg: Message)
  {
    report("In processSendLocally", Severity.Trace)

    try {
      super.processPublicSendLocally(cnxn, msg)

      if (msg.channelType == ChannelType.Request){
        if (msg.channel == Channel.Referral) {
          handlePublicReferralRequestChannel(cnxn, msg)
        }
        else {
          super.processPublicSendLocally(cnxn, msg)
        }
      }
    }
    catch {
      case e : Throwable => {
        report("Exception in processPublicSendLocally: ", e, Severity.Error)
      }
    }
 }
}