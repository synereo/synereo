package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.login._
import com.protegra_ati.agentservices.core.schema._
import java.util.UUID
import org.joda.time.{DateTime, Instant}
import verifier._
import scala.concurrent.ops._
import com.protegra_ati.agentservices.core.schema.util._

trait Auditing {
self: BasePlatformAgent with Storage =>

  def logDataRequested(cnxn: AgentCnxnProxy, auditItem: AuthorizedContentAuditItem, msg: Message)
  {
    logAuditItem(cnxn, generateAuditLogDetailRequested(auditItem, cnxn, msg.originCnxn))
  }

  def logDataApproved(cnxn: AgentCnxnProxy, auditItem: AuthorizedContentAuditItem, msg: Message)
  {
    logAuditItem(cnxn, generateAuditLogDetailApproved(auditItem, cnxn, msg.originCnxn))
  }

  def logDataViewed(cnxn: AgentCnxnProxy, data: Data, requestingCnxn: AgentCnxnProxy)
  {
    spawn {
      //we need to grab the DisclosedData object for the cnxn to determine
      //what the requestingCnxn has access to
     // val searchItem = SearchFactory.getAuthorizedContentAuditSearchItem(data)
      fetch[ AuthorizedContentAuditItem ](_dbQ, cnxn, new AuthorizedContentAuditItem ().toSearchKey, handleLogDataViewedFetchAuthorizedContent(_: AgentCnxnProxy, _: AuthorizedContentAuditItem, data, requestingCnxn))
    }
  }

  def handleLogDataViewedFetchAuthorizedContent(cnxn: AgentCnxnProxy, authorizedContentItem: AuthorizedContentAuditItem, data: Data, requestingCnxn: AgentCnxnProxy)
  {
    data match {
      case x: ExcludeFromAudit => {
      }
      case _ => {
        logAuditItem(cnxn, generateAuditLogDetailRequestSatisfied(data, authorizedContentItem, cnxn, requestingCnxn))
      }
    }
  }

  def logAuditItem(cnxn: AgentCnxnProxy, logDetail: String)
  {
    spawn {
      val item = new AuditLogItem("", new DateTime())
      item.detail = logDetail
      var oldItem: AuditLogItem = null
      updateData(cnxn, item, oldItem)
    }
  }
  /*TODO Task: internalisation of messages created by methods below
    Possible Solution:
    Step 1 instead to return strings to return RawMessageWrapper-Objects (RawMessage -is just a proposal for the class name)
    Step 2 how RawMessageWrapper should looks like:
       class RawMessage (mainMessageKey:String, gapFillingSubMessageKeys:List[String] or Array of Strings or Objects (java-Objects) )
                        getLocalizedMessage(currentLocale:Locale):String{
                        /*
                        a) using ResourceBundle-Instance gets the localized message corresponding to the 'mainMessageKey'
                        b) Best solution to mark the gaps in the localized text via {0}, {1}, ...{i}.
                        ResourceBundle messages =  ResourceBundle.getBundle("MessageBundle",currentLocale);
                        MessageFormat formatter = new MessageFormat("");
                        formatter.setLocale(currentLocale);
                        formatter.applyPattern(messages.getString("template"));

                        everything from the gapFillingSubMessageKeys has to be also retrived and stored in a Object -Arry 'messageArguments'
                        Object[] messageArguments = .....
                        // and finally whole message with filled gaps will be buld
                        String output = formatter.format(messageArguments);
                        */
                        }
      }
  */
  //??? Message factory?
  def generateAuditLogDetailRequestSatisfied(data: Data, authorizedContentItem: AuthorizedContentAuditItem, cnxn: AgentCnxnProxy, requestingCnxn: AgentCnxnProxy): String =
  {
    data.authorizedFieldNames(authorizedContentItem) + " fields of your " + data.getDisplayName() + " were viewed."
  }

  def generateAuditLogDetailRequested(authorizedContentItem: AuthorizedContentAuditItem, cnxn: AgentCnxnProxy, originatingCnxn: AgentCnxnProxy): String =
  {
    "Permission to view your " + authorizedContentItem.objectType.fromCamelCase + " was requested."
  }

  def generateAuditLogDetailApproved(authorizedContentItem: AuthorizedContentAuditItem, cnxn: AgentCnxnProxy, originatingCnxn: AgentCnxnProxy): String =
  {
    "You approved viewing of your " + authorizedContentItem.objectType.fromCamelCase
  }

  def generateAuditLogVerifyPermissionRequested(msg: VerifyPermissionRequest): String =
  {
    "Verification that " + msg.claimKey + " is " + msg.claimData.toString + " requested by " + msg.relyingAgentDescription + " for reason: " + msg.reason
  }

  def generateAuditLogAutoApproveVerifyPermission(msg: VerifyPermissionRequest): String =
  {
    "Verification that " + msg.claimKey + " is " + msg.claimData.toString + " has been auto-approved for " + msg.relyingAgentDescription
  }

  def generateAuditLogVerifyPermissionResponse(request: VerifyPermissionRequest, response: VerifyPermissionResponse): String =
  {
    if ( response.isPermissionGranted ) {
      "You granted permission for " + request.relyingAgentDescription + " to verify that " + request.claimKey + " is " + request.claimData
    }
    else {
      "You denied permission for " + request.relyingAgentDescription + " to verify that " + request.claimKey + " is " + request.claimData
    }
  }

  def generateAuditLogSelectVerifierResponse(request: SelectVerifierRequest, response: SelectVerifierResponse): String =
  {
    "You selected the verifier " + response.verifier.name + " for " + request.claimObject + "." + request.claimField + " for " + request.relyingAgentDescription
  }


  def generateAuditLogVerifyResponse(response: VerifyResponse): String =
  {
    if ( response.isVerified ) {
      response.claimKey + " for " + response.alias + " has been verified."
    }
    else {
      response.claimKey + " for " + response.alias + " has not been verified"
    }
  }

  def generateAuditLogSelectVerifierRequest(request: SelectVerifierRequest): String =
  {
    "Verifier selection has been requested for " + request.claimObject + "." + request.claimField + " from " + request.relyingAgentDescription
  }

  def generateAuditLogGetClaimRequest(request: GetClaimRequest): String =
  {
    "A claim has been requested for: " + request.claimObject + "." + request.claimField
  }

  def generateAuditLogGetClaimResponse(request: GetClaimResponse): String =
  {
    "A claim has been received for " + request.claimObject + "." + request.claimField
  }

  def generateAuditLogVerifyContentRequest(request:VerifyContentRequest): String = {
    "A request to verify " + request.contentVerifier.claimKey + " has been received.";
  }
}