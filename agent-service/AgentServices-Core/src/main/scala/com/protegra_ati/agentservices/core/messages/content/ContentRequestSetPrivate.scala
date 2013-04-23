package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._

/* User: jklassen
*/

trait ContentRequestSetPrivate {
  self:AgentHostStorePlatformAgent =>

  def listenPrivateContentRequest(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode )
      listen(_privateQ, cnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, handlePrivateContentRequestChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Content, ChannelType.Request, ChannelLevel.Private, handlePrivateContentRequestChannel(cnxn, _: Message))
  }

  def handlePrivateContentRequestChannel( cnxn: AgentCnxnProxy, msg: Message) =
  {
    msg.channelLevel = Some(ChannelLevel.Public)
    //we have one case where we need to help a message out
    //setContentAdminRequest where it is a new user and therefore
    //noone is yet listening to process the messages.  The UI doesn't know
    //about the _storeCnxn so we switch it up here
    msg match {
      case x:SetContentAdminRequest => {
        msg.targetCnxn = _storeCnxn
      }
      case _ =>{}
    }
    send(_publicQ, msg.targetCnxn, msg)
  }
}