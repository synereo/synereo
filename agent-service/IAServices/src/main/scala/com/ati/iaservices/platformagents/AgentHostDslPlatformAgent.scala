package com.ati.iaservices.platformagents

import com.ati.iaservices.messages.content.DslContentResponseSetPrivate
import com.protegra_ati.agentservices.core.messages.admin.RegistrationRequest
import com.protegra_ati.agentservices.core.messages.invitation.InvitationResponse
import com.protegra_ati.agentservices.core.messages.{ChannelLevel, Message}
import com.protegra_ati.agentservices.core.platformagents.{AgentHostUIPlatformAgent => CoreAgentHostUIPlatformAgent}
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.protegra_ati.agentservices.store.util.Severity
import com.ati.iaservices.messages.referral.ReferralResponseSetPrivate
import com.ati.iaservices.messages.notification.NotificationResponseSetPrivate

class AgentHostDslPlatformAgent extends CoreAgentHostUIPlatformAgent
with ReferralResponseSetPrivate
with NotificationResponseSetPrivate
with DslContentResponseSetPrivate {
  override def init(config: ConfigurationManager) {
    initPrivate(config)
    initApps(config)
  }

  override def loadQueues() {
    loadPrivateQueue()
  }

  override def startListening() {
    report("IN THE DSL PA LISTEN", Severity.Trace)
    listenPrivate(_cnxnUIStore)
  }

  override def listenPrivate(cnxn: AgentCnxnProxy) {
    listenPrivateReferralResponses(cnxn)
    listenPrivateNotificationResponse(cnxn)
    listenPrivateContentResponse(cnxn)
    listenPrivateInvitationConsumerResponses(cnxn)
  }

  override def send(msg: Message) {
    report("AgentDSL sending msg on private queue with msg id: " + msg.ids.id.toString + " and parent id: " + msg.ids.parentId.toString)

    msg match {
      case x: RegistrationRequest => {
        register(x)
      }

      case _ => {
        msg.channelLevel = Some(ChannelLevel.Private)

        if (!msg.isInstanceOf[InvitationResponse]) {
          msg.originCnxn = msg.targetCnxn
        }

        if (isPrivateKVDBNetworkMode) {
          send(_privateQ, _cnxnUIStore, msg)
        } else {
          sendRabbit(_privateRabbitConfig, _cnxnUIStore, msg)
        }
      }
    }
  }
}
