package com.ati.iaservices.messages.content

import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.protegra_ati.agentservices.core.messages.content.ContentResponseSetPrivate
import com.protegra_ati.agentservices.core.messages.{Response, Message}
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.store.util.Severity

trait DslContentResponseSetPrivate {
  self: AgentHostDslPlatformAgent with ContentResponseSetPrivate =>

  override def handleContentResponseChannel(cnxn: AgentCnxnProxy, msg: Message) = {
    report("entering handleContentResponse for connection:" + cnxn.toString, Severity.Trace)

    report("=================== HANDLE CONTENT RESPONSE ===========, msg is :" + msg.toString, Severity.Debug)

    msg match {
      case x: Message with Response => {
        report("in handleContentResponse triggering event for " + x, Severity.Trace)
        //triggerEvent(x.generateEvent())
      }
      case _ => {}
    }
  }
}
