package com.protegra_ati.agentservices.core.messages.introduction

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._

trait IntroductionResponseSetConsumerPrivate
{
  self: AgentHostUIPlatformAgent =>

  def listenPrivateIntroductionConsumerResponses(cnxn: AgentCnxn) =
  {
    listen(_privateQ, cnxn, Channel.Introduction, Some(ChannelRole.Consumer), ChannelType.Response, ChannelLevel.Private, handleIntroductionResponseChannel(_: AgentCnxn, _: Message))
  }

  def handleIntroductionResponseChannel(cnxn: AgentCnxn, msg: Message)
  {
    report("=================== HANDLE INVITATION RESPONSE ===========, msg is :" + msg.toString + " cnxn is: " + cnxn.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[ Response ] => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
      case _ => {report("message does not generate events", Severity.Trace)}
    }

  }


}