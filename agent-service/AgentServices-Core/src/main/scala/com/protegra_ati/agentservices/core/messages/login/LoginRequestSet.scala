package com.protegra_ati.agentservices.core.messages.login

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.util.Severity
import com.protegra_ati.agentservices.core.schema.util._

trait LoginRequestSet {
  self:AgentHostStorePlatformAgent =>

  def listenPublicLoginRequest(cnxn: AgentCnxn) = {
    listen(_publicQ, cnxn, Channel.Security, ChannelType.Request, ChannelLevel.Public, handlePublicSecurityRequestChannel(_: AgentCnxn, _: Message))
  }

  private def handlePublicSecurityRequestChannel(cnxn: AgentCnxn, msg: Message) =
  {
    report("entering handlePrivateSecurityRequestChannel in StorePlatform", Severity.Trace)
    msg match {
      case x: SetLoginRequest => {
        //save the loginToken to all the connections the user writes to
        processSetLoginRequest(x)

      }
      case _ => report("***********************not doing anything in handlePrivateSecurityRequestChannel", Severity.Error)
    }
    report("exiting handlePrivateSecurityRequestChannel in StorePlatform", Severity.Trace)

  }

  private def processSetLoginRequest(msg: SetLoginRequest)
  {
    val loginToken = msg.loginToken
    //for each connection in the user's data silo, clear out any existing token and save the new login token
    updateDataBySearch(msg.targetCnxn, new  LoginToken (), loginToken)
    val connectionSearch = new Connection ()
    fetch[ Connection ](_dbQ, msg.targetCnxn, connectionSearch.toSearchKey, handleSetLoginByConnectionFetch(_: AgentCnxn, _: Connection, loginToken))

    val response = new SetLoginResponse(msg.ids.copyAsChild(), msg.eventKey.copy())
    response.originCnxn = msg.originCnxn
    response.targetCnxn = msg.targetCnxn
    send(_publicQ, msg.originCnxn, response)
  }

  private def handleSetLoginByConnectionFetch(cnxn: AgentCnxn, userConnection: Connection, token: LoginToken)
  {
    updateDataBySearch(userConnection.writeCnxn, new LoginToken (), token)
  }

}