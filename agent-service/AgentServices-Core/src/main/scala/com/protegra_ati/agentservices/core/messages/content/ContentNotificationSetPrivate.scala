package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._

trait ContentNotificationSetPrivate {
  self:AgentHostUIPlatformAgent =>

  def listenPrivateContentNotification(cnxn: AgentCnxnProxy) =
  {
    listen(_privateQ, cnxn, Channel.Content, ChannelType.Notification, ChannelLevel.Private, handlePrivateContentNotificationChannel(_: AgentCnxnProxy, _: Message))
//    listen(_privateQ, cnxn, Channel.Permission, ChannelType.Notification, ChannelLevel.Private, handleNotificationsChannel(_: AgentCnxnProxy, _: Message))
  }

  def handlePrivateContentNotificationChannel( cnxn: AgentCnxnProxy, msg: Message) =
  {
     report("=================== HANDLE CONTENT NOTIFICATIONS ===========, msg is :" + msg.toString +" cnxn is: " + cnxn.toString, Severity.Debug)
     msg match {
       case x: Message with EventProducer[Notification] => {
         report("in handleNotificationsChannel triggering event for " + x, Severity.Trace)
         triggerEvent(x.generateEvent())
       }
       case _ => {}
     }
  }

}