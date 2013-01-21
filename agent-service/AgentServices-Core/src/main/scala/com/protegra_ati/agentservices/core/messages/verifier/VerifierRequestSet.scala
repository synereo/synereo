package com.protegra_ati.agentservices.core.messages.verifier

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.platformagents.behaviors._
import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util.Severity
import java.util.UUID
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core.schema.util._

trait VerifierRequestSet {
  self:AgentHostStorePlatformAgent =>

  def listenPublicVerifierRequest(cnxn: AgentCnxnProxy) = {
    listen(_publicQ, cnxn, Channel.Verify, ChannelType.Request, ChannelLevel.Public, handleVerifyRequestChannel(_:AgentCnxnProxy, _:Message))
  }

  protected def handleVerifyRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    report("entering handleVerifyChannel in VerifierPlatformAgent", Severity.Trace)
//    if (!_processedMessages.contains(msg.ids.id )) {
//      _processedMessages = _processedMessages + msg.ids.id
      msg match {

        //verifier
        case x: VerifyRequest => {
          fetchVerifierConnection(x)
        }
        //claiming agent
        case x: GetClaimRequest => {
          processGetClaimRequest(x)
        }
          //claiming agent
        case x: VerifyPermissionRequest => {
          processVerifyPermissionRequest(cnxn, x)
        }
        case x: VerifyContentRequest => {
          processVerifyContentRequest(x)
        }
        case _ => report("***********************not doing anything in handleVerifyChannel", Severity.Error)
      }
//    }
    report("exiting handleVerifyChannel in VerifierPlatformAgent", Severity.Trace)
  }

  protected def processGetClaimRequest(request: GetClaimRequest) =
  {
    report("entering processGetClaimRequest in StorePlatform", Severity.Trace)

    _forwardedMessages.put(request.ids.conversationId , request)
    val selectVerifierRequest = SelectVerifierRequest(request.ids.copyAsChild(), request.eventKey.copy(), request.verifierList, request.claimObject, request.claimField, request.relyingAgentDescription)
    selectVerifierRequest.targetCnxn = request.targetCnxn
    val persistedRequest = PersistedRequest("SelectVerifierRequest", selectVerifierRequest, new DateTime()) //change to class
    var oldData: PersistedRequest = null
    updateDataById(request.targetCnxn, persistedRequest)
    logAuditItem(request.targetCnxn, generateAuditLogGetClaimRequest(request))
    logAuditItem(request.targetCnxn, generateAuditLogSelectVerifierRequest(selectVerifierRequest))
    notifyUser(request.notifyCnxn, selectVerifierRequest)

    report("exiting processGetClaimRequest in StorePlatform", Severity.Trace)
  }

  protected def fetchVerifierConnection(msg:VerifyRequest) = {
    report("entering fetchVerifierConnection in VerifierPlatformAgent", Severity.Trace)
    _verifyRequests.put(msg.ids.conversationId , msg)
    val search = SystemDataFactory.SEARCH_ALL_CONNECTION
    fetch(_dbQ, msg.targetCnxn, search.toSearchKey, fetchClaimingAgentConnection(_:AgentCnxnProxy, _:SystemData[Connection], msg))
    report("exiting fetchVerifierConnection in VerifierPlatformAgent", Severity.Trace)
  }

  protected def fetchClaimingAgentConnection(cnxn:AgentCnxnProxy, systemConnection:SystemData[Connection], verifyRequest:VerifyRequest) {
    report("entering fetchClaimingAgentConnection in VerifierPlatformAgent", Severity.Trace)
    val search = new AliasConnection(verifyRequest.alias, "")
    fetch(_dbQ, systemConnection.data.readCnxn, search.toSearchKey, fetchConnection(_:AgentCnxnProxy, _:Data, verifyRequest, systemConnection.data.readCnxn))
    report("exiting fetchClaimingAgentConnection in VerifierPlatformAgent", Severity.Trace)
  }

  protected def fetchConnection(cnxn:AgentCnxnProxy, data:Data, verifyRequest:VerifyRequest, selfCnxn:AgentCnxnProxy) = {
    report("entering fetchConnection in VerifierPlatformAgent", Severity.Trace)

    data match {
      case x:AliasConnection => {
        val search = new Connection()
        search.id = x.connectionId.toString // DON'T CHANGE here to factory clall !!!
//        search.setSearchFieldValue("id", x.connectionId.toString)
        fetch(_dbQ, selfCnxn, search.toSearchKey, requestVerifyPermission(_:AgentCnxnProxy, _:Data, verifyRequest, selfCnxn))
    }
      case _ => {
        report("fetchConnection - unexpected data", Severity.Trace)
      }
    }

    report("exiting fetchConnection in VerifierPlatformAgent", Severity.Trace)
  }

  protected def requestVerifyPermission(cnxn:AgentCnxnProxy, data:Data, verifyRequest:VerifyRequest, selfCnxn:AgentCnxnProxy) {
    report("entering requestVerifyPermission in VerifierPlatformAgent", Severity.Trace)

    data match {
      case x:Connection => {
          val verifyPermissionRequest = VerifyPermissionRequest(verifyRequest.ids.copyAsChild(), null, verifyRequest.claimKey, verifyRequest.claimData, verifyRequest.reason, verifyRequest.relyingAgentDescription)
          verifyPermissionRequest.originCnxn = x.readCnxn
          verifyPermissionRequest.targetCnxn = x.readCnxn //location of the content verifier
          report("sending " + verifyPermissionRequest.toString + " on cnxn: " + x.writeCnxn.toString + " with originCnxn: " + verifyPermissionRequest.originCnxn.toString, Severity.Info)
          send(_publicQ, x.writeCnxn, verifyPermissionRequest)
      }
      case _ => {
          report("requestVerifyPermission - unexpected data type", Severity.Error)
      }
   }

   report("exiting requestVerifyPermission in VerifierPlatformAgent", Severity.Trace)
  }

  protected def processVerifyPermissionRequest(cnxn: AgentCnxnProxy, request: VerifyPermissionRequest) =
  {
    report("entering processVerifyPermissionRequest in StorePlatform", Severity.Trace)

    val search = new  ContentVerifier("", "", request.claimKey, "", "", "", "")
    
    fetch[ ContentVerifier ](_dbQ, request.targetCnxn, search.toSearchKey, handleContentVerifierFetch(_: AgentCnxnProxy, _: ContentVerifier, request, cnxn))

    report("exiting processVerifyPermissionRequest in StorePlatform", Severity.Trace)
  }

  protected def handleContentVerifierFetch(targetCnxn: AgentCnxnProxy, data: ContentVerifier, request: VerifyPermissionRequest, cnxn: AgentCnxnProxy) =
  {
    report("entering handleContentVerifierFetch in StorePlatform", Severity.Trace)

    if ( data.autoApprove.toBoolean ) {
      report("handleContentVerifierFetch - found auto-approve content verifier", Severity.Trace)
      val response = new VerifyPermissionResponse(request.ids.copyAsChild, true)
      send(_publicQ, request.originCnxn, response)
      logAuditItem(cnxn, generateAuditLogAutoApproveVerifyPermission(request))
    }
    else {
      report("handleContentVerifierFetchfound manual-approve content verifier", Severity.Trace)
      val persistedRequest = PersistedRequest("VerifyPermissionRequest", request, new DateTime()) //change to class
      var oldData: PersistedRequest = null
      updateDataById(cnxn, persistedRequest)
      logAuditItem(cnxn, generateAuditLogVerifyPermissionRequested(request))
      notifyUser(request.targetCnxn, request)
    }

    //hack for testing
    //  val response = VerifyPermissionResponse(request.ids.copyAsChild, true)
    //  report("sending verify permission response - " + request.originCnxn.toString + " : " + response.toString, Severity.Trace)
    //  send(_publicQ, request.originCnxn, response)

    report("exiting handleContentVerifierFetch in StorePlatform", Severity.Trace)
  }

  protected def processVerifyContentRequest(request: VerifyContentRequest) =
  {
    report("entering processVerifyContentRequest", Severity.Trace)

    val persistedRequest = PersistedRequest("VerifyContentRequest", request, new DateTime()) //change to class
    var oldData: PersistedRequest = null
    updateDataById(request.targetCnxn, persistedRequest)
    logAuditItem(request.targetCnxn, generateAuditLogVerifyContentRequest(request))
    notifyUser(request.claimingAgentCnxnProxy, request)

    report("exiting processVerifyContentRequest in StorePlatform", Severity.Trace)
  }

//  def processVerifyRequest(msg: VerifyRequest) =
//  {
//    report("entering processVerifyRequest in StorePlatform", Severity.Trace)
//    _forwardedMessages.put(msg.ids.conversationId, msg)
//    msg.channelLevel = Some(ChannelLevel.Public)
//    send(_publicQ, msg.targetCnxn, msg)
//
//
//
//
//    //  HACK FOR TESTING
//    //    request.originCnxn = _cnxnRAVerifier
//    //    send(_publicQ, _cnxnRAVerifier, request)
//    //    logVerifyRequest(_cnxnRAVerifier, request)
//    report("exiting processVerifyRequest in StorePlatform", Severity.Trace)
//  }

}