package com.protegra_ati.agentservices.core.messages.verifier

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util.Severity
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.schema.util._

trait VerifierResponseSet
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicVerifierResponse(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Verify, ChannelType.Response, ChannelLevel.Public, handleVerifyResponseChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handleVerifyResponseChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleVerifyChannel in VerifierPlatformAgent", Severity.Trace)
    //    if (!_processedMessages.contains(msg.ids.id)) {
    //      _processedMessages = _processedMessages + msg.ids.id
    msg match {

      case x: VerifyPermissionResponse => {
        processVerifyPermissionResponse(cnxn, x)
      }
      case x: VerifyResponse => {
        logAuditItem(cnxn, generateAuditLogVerifyResponse(x))
        sendPrivateMessage(cnxn, x)
      }
      case x: SelectVerifierResponse => {
        processSelectVerifierResponse(x)
      }
      case x: GetClaimResponse => {
        sendPrivateMessage(cnxn, x)
      }
      case x: VerifyContentResponse => {
        processVerifyContentResponse(cnxn, x)
      }
      case _ => report("***********************not doing anything in handleVerifyChannel", Severity.Error)
    }
    //    }
    report("exiting handleVerifyChannel in VerifierPlatformAgent", Severity.Trace)
  }

  protected def sendPrivateMessage(cnxn: AgentCnxnProxy, msg: Message)
  {
    msg.channelLevel = None
//    send(_privateQ, msg.originCnxn, msg)
  }

  protected def processVerifyPermissionResponse(cnxn: AgentCnxnProxy, msg: VerifyPermissionResponse) =
  {
    report("entering fetchVerifierConnection in VerifierPlatformAgent", Severity.Trace)
    //TODO it is necessary to create 'companion objects' with a reasonable named factory methods to create reusable singletons of default constructed Data objects like connection()
    val search = SystemDataFactory.SEARCH_ALL_CONNECTION
    fetch(_dbQ, cnxn, search.toSearchKey, sendVerifyPermissionResponse(_: AgentCnxnProxy, _: SystemData[ Connection ], msg))
    report("exiting fetchVerifierConnection in VerifierPlatformAgent", Severity.Trace)
  }

  protected def sendVerifyPermissionResponse(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], msg: VerifyPermissionResponse)
  {
    report("entering processVerifyPermissionResponse in VerifierPlatformAgent", Severity.Trace)

    val verifyRequest = _verifyRequests.get(msg.ids.conversationId)
    if ( msg.isPermissionGranted ) {
      report("verify permission granted!", Severity.Trace)
      val search = new VerifiedData(verifyRequest.alias, verifyRequest.claimKey, "")
      fetch(_dbQ, systemConnection.data.readCnxn, search.toSearchKey, processClaim(_: AgentCnxnProxy, _: Data, verifyRequest: VerifyRequest, msg))
    }
    else {
      report("verify permission denied!", Severity.Trace)
      val response = VerifyResponse(verifyRequest.ids.copyAsChild(), verifyRequest.eventKey.copy(), verifyRequest.alias, verifyRequest.claimKey, false)
      response.originCnxn = verifyRequest.originCnxn
      send(_publicQ, verifyRequest.relyingAgentCnxnProxy, response)
    }
    report("exiting processVerifyPermissionResponse in VerifierPlatformAgent", Severity.Trace)
  }

  protected def processClaim(cnxn: AgentCnxnProxy, data: Data, verifyRequest: VerifyRequest, parent: Message) =
  {
    report("entering processClaim in VerifierPlatformAgent", Severity.Trace)
    data match {
      case x: VerifiedData => {
        val response = VerifyResponse(parent.ids.copyAsChild(), verifyRequest.eventKey.copy(), verifyRequest.alias, verifyRequest.claimKey, verifyRequest.claimData == x.value)
        response.originCnxn = verifyRequest.originCnxn
        send(_publicQ, verifyRequest.relyingAgentCnxnProxy, response)
        val reportMsg = if ( verifyRequest.claimData == x.value ) "claim verified!" else "claim rejected!"
        report(reportMsg, Severity.Trace)
        _verifyRequests.remove(parent.ids.conversationId)
      }
      case _ => {
        report("processClaim: unexpected data type - msg id: " + parent.ids.id.toString, Severity.Error)
      }
    }
    report("exiting processClaim in VerifierPlatformAgent", Severity.Trace)
  }


  def processSelectVerifierResponse(response: SelectVerifierResponse) =
  {
    report("entering processSelectVerifierResponse", Severity.Trace)

    val request = _forwardedMessages.get(response.ids.conversationId)

    val getClaimResponse = GetClaimResponse(response.ids.copyAsChild(), response.eventKey.copy(), response.claimObject, response.claimField, response.verifier)
    getClaimResponse.targetCnxn = response.targetCnxn
    getClaimResponse.originCnxn = request.originCnxn
    val search = new ContentVerifier("", "", response.claimObject + "." + response.claimField, "", "", "false", "")
    fetch[ ContentVerifier ](_dbQ, response.targetCnxn, search.toSearchKey, handleFetchClaimContentVerifier(_: AgentCnxnProxy, _: ContentVerifier, getClaimResponse))
    report("entering processSelectVerifierResponse", Severity.Trace)
  }

  def handleFetchClaimContentVerifier(cnxn: AgentCnxnProxy, contentVerifier: ContentVerifier, getClaimResponse: GetClaimResponse) =
  {
    getClaimResponse.alias = contentVerifier.claimingAgentAlias
    val search = getSearch(getClaimResponse.claimObject)
    fetch[ Data ](_dbQ, cnxn, search.toSearchKey, handleFetchClaimContent(_: AgentCnxnProxy, _: Data, getClaimResponse))
  }

  protected def getSearch(className: String) =
  {
    //    val claimClass = Class.forName(getClaimResponse.claimObject.toCamelCase)
    //    TODO: figure out how to create the search generically
    if ( className.toCamelCase == "profile" ) {
      Profile.SEARCH_ALL
    } else {
      // TODO null has to be replaced with some kind null-object
      null
    }
  }

  def handleFetchClaimContent(cnxn: AgentCnxnProxy, data: Data, getClaimResponse: GetClaimResponse) =
  {
    getClaimResponse.claimValue = data.getValue(getClaimResponse.claimField)
    send(_publicQ, getClaimResponse.targetCnxn, getClaimResponse)
    logAuditItem(getClaimResponse.targetCnxn, generateAuditLogGetClaimResponse(getClaimResponse))
  }

  def processVerifyContentResponse(cnxn: AgentCnxnProxy, msg: VerifyContentResponse) =
  {
    val search = SystemDataFactory.SEARCH_ALL_CONNECTION
    fetch(_dbQ, cnxn, search.toSearchKey, sendVerifyContentResponse(_: AgentCnxnProxy, _: SystemData[ Connection ], msg))
  }

  def sendVerifyContentResponse(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], response: VerifyContentResponse) =
  {
    val newContentVerifier = response.contentVerifier.copy()
    if ( response.isApproved ) {
      newContentVerifier.status = "APPROVED" // TODO: need the enum here
      logAuditItem(response.targetCnxn, "Request to verify " + response.contentVerifier.claimKey + " has been approved")
    } else {
      newContentVerifier.status = "DENIED"
      logAuditItem(response.targetCnxn, "Request to verify " + response.contentVerifier.claimKey + " has been denied")
    }
    setContentForSelfAndAllConnections(systemConnection.data.readCnxn, response.ids, response.eventKey, newContentVerifier, response.contentVerifier)
  }

}