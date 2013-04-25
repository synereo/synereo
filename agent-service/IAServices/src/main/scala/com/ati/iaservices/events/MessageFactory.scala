package com.ati.iaservices.events

import com.protegra_ati.agentservices.core.messages.content.{SetContentRequest, GetContentRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.schema.{Post, Data, AgentCnxnProxy}
import com.protegra_ati.agentservices.core.messages.admin.RegistrationRequest
import com.protegra_ati.agentservices.core.messages.Identification
import java.util.UUID
import com.ati.iaservices.messages.referral.CreateReferralRequest

object MessageFactory {
  def createGetContentRequest(agentSessionId: UUID, tag: String, queryObject: Data, target: AgentCnxnProxy): GetContentRequest = {
    val eventKey: EventKey = EventKey(agentSessionId, tag)
    val msg: GetContentRequest = GetContentRequest(eventKey, queryObject)
    msg.setTargetCnxn(target)
    msg
  }

  def createSetContentRequest(agentSessionId: UUID, tag: String, newData: Data, target: AgentCnxnProxy): SetContentRequest = {
    val eventKey: EventKey = EventKey(agentSessionId, tag)
    val msg: SetContentRequest = SetContentRequest(eventKey, newData, null)
    msg.setTargetCnxn(target)
    msg
  }

  def createRegistrationRequest(agentSessionId: UUID, tag: String, appId: UUID, alias: String): RegistrationRequest = {
    val eventKey: EventKey = EventKey(agentSessionId, tag)
    val msg = RegistrationRequest(eventKey, appId, alias)
    msg
  }

  def createCreateReferralRequest( agentSessionId: UUID,
                                   tag: String,
                                   invitationConnectionId_A: String,
                                   invitationConnectionId_B: String,
                                   alias_A: String,
                                   alias_B: String,
                                   post_A: Post,
                                   post_B: Post,
                                   target: AgentCnxnProxy): CreateReferralRequest = {
    val eventKey: EventKey = EventKey(agentSessionId, tag)
    val msg: CreateReferralRequest = CreateReferralRequest(
      new Identification(),
      eventKey,
      invitationConnectionId_A,
      invitationConnectionId_B,
      alias_A,
      alias_B,
      post_A,
      post_B)
    msg.setTargetCnxn(target)
    msg
  }
}
