package com.ati.iaservices.events

import com.protegra_ati.agentservices.core.messages.content.{SetContentRequest, GetContentRequest}
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.schema.{Post, Data, AgentCnxnProxy}
import com.protegra_ati.agentservices.core.messages.admin.RegistrationRequest
import com.protegra_ati.agentservices.core.messages.Identification
import java.util.UUID
import com.ati.iaservices.messages.referral.CreateReferralRequest
import com.protegra_ati.agentservices.core.messages.invitation.{InvitationRequest, InvitationResponse}

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

  def createCreateReferralRequest( agentSessionId: UUID, tag: String, target: AgentCnxnProxy )
                                 ( invitationConnectionId_A: String, alias_A: String, post_A: Post )
                                 ( invitationConnectionId_B: String, alias_B: String, post_B: Post ): CreateReferralRequest = {
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

  def createInvitationResponse(agentSessionId: UUID, tag: String, target: AgentCnxnProxy)
                              (category: String, connectionType: String, connectionName: String, post: Post, conversationThread: java.util.List[ Post ], accept: Boolean, roleBasedAlias: String, invitationRequest: InvitationRequest )
  : InvitationResponse = {
    val eventKey: EventKey = EventKey(agentSessionId, tag)
    val msg: InvitationResponse = InvitationResponse(
      new Identification(),
      eventKey,
      category,
      connectionType,
      connectionName,
      post,
      conversationThread,
      accept,
      roleBasedAlias,
      invitationRequest
    )
    msg.setTargetCnxn(target)
    msg
  }

}
