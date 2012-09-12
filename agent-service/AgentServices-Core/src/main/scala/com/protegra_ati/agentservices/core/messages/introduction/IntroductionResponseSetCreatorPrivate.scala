package com.protegra_ati.agentservices.core.messages.introduction

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._


trait IntroductionResponseSetCreatorPrivate
{
  self: AgentHostStorePlatformAgent =>

  def listenPrivateIntroductionCreatorResponses(cnxn: AgentCnxn) =
  {
    listen(_privateQ, cnxn, Channel.Introduction, Some(ChannelRole.Creator), ChannelType.Response, ChannelLevel.Private, handleIntroductionResponseChannel(_: AgentCnxn, _: Message))
  }

  def handleIntroductionResponseChannel(cnxn: AgentCnxn, msg: Message)
  {
    report("entering handlePrivateIntroductionResponseChannel", Severity.Trace)

    msg match {

        //no special handling needed for IntroResponses
//      case x: IntroductionResponse => {
//        singleSend(_publicQ, x.targetCnxn, x)
//      }

      case _ => {
        //just putting it on the publicQ
        msg.channelLevel = Some(ChannelLevel.Public)
        send(_publicQ, msg.targetCnxn, msg)
      }
    }

    report("exiting handlePrivateIntroductionRequestChannel", Severity.Trace)
  }


}