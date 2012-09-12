package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.login._
import com.protegra_ati.agentservices.core.schema._
import content._
import org.joda.time.{DateTime, Instant}
import com.protegra.agentservicesstore.util.Severity
import verifier._
import com.protegra_ati.agentservices.core.schema.util._

trait Notifications {
  self: BasePlatformAgent with Storage with Private =>

  def notifyUser(cnxn: AgentCnxn, msg: Message)
  {
    //check to see if there is a logged in token for the target cnxn
    //if there is, send a notification
    val loginSearch =new LoginToken ()
    fetch[ LoginToken ](_dbQ, cnxn, loginSearch.toSearchKey, handleLoginSearchFetch(_: AgentCnxn, _: LoginToken, msg))
  }

  def handleLoginSearchFetch(cnxn: AgentCnxn, token: LoginToken, msg: Message)
  {
    //note that this method sends directory on the privateQ which should be
    //fine as the login token should only exist on the PA which is hosting
    //the UI for which the notification is meant
    report("entering handleLoginSearchFetch in StorePlatform", Severity.Trace)
    val currentTime = new DateTime()

    msg match {
      case x: Message with NotificationProducer => {
        val notification = x.generateNotification(token.key)
        send(_privateQ, token.currentUICnxn.readCnxn, notification)
      }
//      case x: VerifyPermissionRequest => {
//        val notification = new VerifyPermissionRequiredNotification(token.key)
//        send(_privateQ, token.currentUICnxn.readCnxn, notification)
//      }
//      case x: SelectVerifierRequest => {
//        val notification = new SelectVerifierRequestNotification(token.key)
//        send(_privateQ, token.currentUICnxn.readCnxn, notification)
//      }
//
//      case x: VerifyContentRequest => {
//        val notification = new VerifyContentRequestNotification(token.key)
//        send(_privateQ, token.currentUICnxn.readCnxn, notification)
//      }

      case _ => {}
    }
    report("exiting handleLoginSearchFetch in StorePlatform", Severity.Trace)
  }

}