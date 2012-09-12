package com.protegra_ati.agentservices.core.messages.content

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._

trait ContentNotificationSetPrivate {
  self:AgentHostUIPlatformAgent =>

  def listenPrivateContentNotification(cnxn: AgentCnxn) =
  {
    listen(_privateQ, cnxn, Channel.Content, ChannelType.Notification, ChannelLevel.Private, handlePrivateContentNotificationChannel(_: AgentCnxn, _: Message))
//    listen(_privateQ, cnxn, Channel.Permission, ChannelType.Notification, ChannelLevel.Private, handleNotificationsChannel(_: AgentCnxn, _: Message))
  }

  def handlePrivateContentNotificationChannel( cnxn: AgentCnxn, msg: Message) =
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