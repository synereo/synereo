package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util.Severity
import com.protegra_ati.agentservices.core.schema._


/* User: jviolago
*/

trait VerifierRequestSetPrivate {
  self:AgentHostStorePlatformAgent =>

  def listenPrivateVerifierResponse(cnxn: AgentCnxnProxy) = {
//    listen(_privateQ, _cnxnUIStore, Channel.Verify, ChannelType.Request, ChannelLevel.Private, handlePrivateVerifyRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  def handlePrivateVerifyRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handlePrivateVerifyRequestChannel in StorePlatform", Severity.Trace)
    msg.channelLevel = Some(ChannelLevel.Public)
    send(_publicQ, msg.targetCnxn, msg)
    report("exiting handlePrivateVerifyRequestChannel in StorePlatform", Severity.Trace)
  }
}