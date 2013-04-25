package com.ati.iaservices.platformagents

/*
 This should only interface with the UI and the AgentHostStorePA for db access, not visible to the public network
 */

import com.protegra_ati.agentservices.core.messages.admin._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.platformagents.{AgentHostUIPlatformAgent => CoreAgentHostUIPlatformAgent}
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.invitation.InvitationResponse

import com.ati.iaservices.messages.referral.ReferralResponseSetPrivate

class AgentHostUIPlatformAgent extends CoreAgentHostUIPlatformAgent

with ReferralResponseSetPrivate
//with NotificationResponseSetPrivate
{
  override def listenPrivate(cnxn: AgentCnxnProxy) =
  {
    listenPrivateReferralResponses(cnxn)
    super.listenPrivate(cnxn)
  }

  override def send(msg: Message)
  {
    report("AgentUI sending msg on private queue with msg id: " + msg.ids.id.toString + " and parent id: " + msg.ids.parentId.toString)

    msg match {
      case x: RegistrationRequest =>
      {
        register(x)
      }
      case _ => {
        msg.channelLevel = Some(ChannelLevel.Private)
        //    msg.originCnxn = _cnxnUIStore
        if (!msg.isInstanceOf[InvitationResponse]){
          msg.originCnxn = msg.targetCnxn
        }
        if ( isPrivateKVDBNetworkMode() )
          send(_privateQ, _cnxnUIStore, msg)
        else
          sendRabbit(_privateRabbitConfig, _cnxnUIStore, msg)
      }
    }
  }

}


