package com.ati.iaservices.messages.notification

import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.util.Severity
import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent

trait NotificationResponseSetPrivate {

  self: AgentHostDslPlatformAgent =>

  def listenPrivateNotificationResponse(cnxn: AgentCnxnProxy) =
  {

    if ( isPrivateKVDBNetworkMode )
      listen(_privateQ, cnxn, Channel.Notification, ChannelType.Response, ChannelLevel.Private, handleNotificationResponseChannel(_: AgentCnxnProxy, _: Message))
    else
      listenRabbit(_privateRabbitConfig, cnxn, Channel.Notification, ChannelType.Response, ChannelLevel.Private, handleNotificationResponseChannel(cnxn, _: Message))
  }

  protected def handleNotificationResponseChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleNotificationResponse for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE Security RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)
    msg match {
      case x: Message with EventProducer[ Response ] => {
        report("in handleSearchResponse triggering event for " + x, Severity.Trace)
        triggerEvent(x.generateEvent())
      }

      case _ => {}
    }

  }

}
