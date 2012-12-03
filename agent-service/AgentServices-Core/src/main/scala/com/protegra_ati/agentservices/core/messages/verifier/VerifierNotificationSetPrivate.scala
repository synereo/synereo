package com.protegra_ati.agentservices.core.messages.verifier

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._

trait VerifierNotificationSetPrivate {
  self:AgentHostUIPlatformAgent =>

  def listenPrivateVerifierNotification(cnxn: AgentCnxnProxy) =
  {
//    listen(_privateQ, cnxn, Channel.Verify, ChannelType.Notification, ChannelLevel.Private, handlePrivateVerifierNotificationChannel(_: AgentCnxnProxy, _: Message))
////    listen(_privateQ, cnxn, Channel.Permission, ChannelType.Notification, ChannelLevel.Private, handleNotificationsChannel(_: AgentCnxnProxy, _: Message))
  }

  def handlePrivateVerifierNotificationChannel( cnxn: AgentCnxnProxy, msg: Message) =
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