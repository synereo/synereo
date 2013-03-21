package com.protegra_ati.agentservices.core.messages.content

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util._
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.util.rabbit.MessageAMQPListener

trait ContentResponseSetPrivate
{
  self: AgentHostUIPlatformAgent =>

  def listenPrivateContentResponse(cnxn: AgentCnxnProxy) =
  {
    if ( isPrivateKVDBNetworkMode() )
      listen(_privateQ, cnxn, Channel.Content, ChannelType.Response, ChannelLevel.Private, handleContentResponseChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Content, ChannelType.Response, ChannelLevel.Private, handleContentResponseChannel(cnxn, _: Message))
  }

  def handleContentResponseChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleContentResponse for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE CONTENT RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)


    msg match {
      case x: Message with EventProducer[Response] => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }
      case _ => {}
    }
  }

}