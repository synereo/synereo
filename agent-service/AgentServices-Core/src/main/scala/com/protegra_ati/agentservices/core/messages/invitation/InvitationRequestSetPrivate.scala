package com.protegra_ati.agentservices.core.messages.invitation

/* User: jklassen
*/
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID


trait InvitationRequestSetPrivate {
  self:AgentHostStorePlatformAgent =>

  def listenPrivateInvitationRequest(cnxn:AgentCnxnProxy) =
  {
    listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Creator), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(_: AgentCnxnProxy, _: Message))
    listen(_privateQ, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Private, handlePrivateInvitationRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  //overriding listen for hosted connections and putting broker listen logic in
  protected def handlePrivateInvitationRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handlePrivateInvitationRequestChannel in ConnectionBroker", Severity.Trace)

    //just putting it on the publicQ
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)

    report("exiting handlePrivateInvitationRequestChannel in ConnectionBroker", Severity.Trace)
  }
}