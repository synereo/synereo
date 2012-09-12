package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.util.Severity

trait IntroductionResponseSet
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicIntroductionConsumerResponses(cnxn:AgentCnxn) =
  {
    listen(_publicQ, cnxn, Channel.Introduction, Some(ChannelRole.Consumer), ChannelType.Response, ChannelLevel.Public, sendPrivate(_:AgentCnxn, _:Message))
  }

  def listenPublicIntroductionCreatorResponses(cnxn: AgentCnxn) =
  {
    listen(_publicQ, cnxn, Channel.Introduction, Some(ChannelRole.Creator), ChannelType.Response, ChannelLevel.Public, handlePublicIntroductionCreatorResponseChannel(_: AgentCnxn, _: Message))
  }

  //there should be nothing on these channels, it's all single listens on public or protected
  //  def listenPublicIntroductionCreatorResponses(cnxn:AgentCnxn) = {
  //
  //  }

  private def handlePublicIntroductionCreatorResponseChannel(cnxn: AgentCnxn, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicIntroductionCreatorResponseChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: CreateIntroductionResponse => {
        //TODO: process
        processCreateIntroductionResponse(cnxn, x)
      }

      case _ => report("***********************not doing anything in handlePublicIntroductionCreatorResponseChannel", Severity.Error)
    }
    report("exiting handlePublicIntroductionCreatorResponseChannel in ConnectionBroker", Severity.Trace)
  }

  private def processCreateIntroductionResponse(cnxnBroker_A: AgentCnxn, createInviteRequest: CreateIntroductionResponse) =
  {
    //send to self to update the state of the intro, then send out 2 InviteRequests. logic should flow through existing Invite package after that
  }

}