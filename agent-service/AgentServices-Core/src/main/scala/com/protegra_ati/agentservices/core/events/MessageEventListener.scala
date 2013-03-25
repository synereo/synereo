/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.events

import com.protegra_ati.agentservices.store.util.Reporting

trait MessageEventListener extends Reporting
{
  def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
  {
    report("registrationResponseReceived, not implemented")
  }
//  def approveRegistrationResponseReceived(e: ApproveRegistrationResponseReceivedEvent) =
//  {
//    report("approveRegistrationResponseReceived, not implemented")
//  }
//  def affiliateResponseReceived(e: AffiliateResponseReceivedEvent) =
//  {
//    report("affiliateResponseReceived, not implemented")
//  }


  def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
  {
    report("in base GetContentResponseReceived, not implemented")
  }

//  def searchResponseReceived(e: SearchResponseReceivedEvent) =
//  {
//    report("in base searchResponseReceived, not implemented")
//  }

  def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
  {
    report("in base setContentResponseReceived, not implemented")
  }

  def setContentAdminResponseReceived(e: SetContentAdminResponseReceivedEvent) =
   {
     report("in base setContentAdminResponseReceived, not implemented")
   }
  def setContentPersistedResponseReceived(e: SetContentPersistedResponseReceivedEvent) =
  {
    report("in base setContentPersistedResponseReceived, not implemented")
  }

  def setContentAuthorizationResponseReceived(e: SetContentAuthorizationResponseReceivedEvent) =
  {
    report("in base setContentAuthorizationResponseReceived, not implemented")
  }

  def deleteContentResponseReceived(e: DeleteContentResponseReceivedEvent) =
  {
    report("in base deleteContentResponseReceived, not implemented")
  }

  def verifyResponseReceived(e: VerifyResponseReceivedEvent) =
  {
    report("in base verifyResponseReceived, not implemented")
  }

  def setLoginResponseReceived(e: SetLoginResponseReceivedEvent) =
  {report("setLoginResponseReceived, not implemented")}

  def getContentAuthorizationRequiredNotificationReceived(e: GetContentAuthorizationRequiredNotificationReceivedEvent) =
  {report("getContentAuthorizationRequiredNotificationReceived, not implemented")}

  def verifyPermissionRequiredNotificationReceived(e: VerifyPermissionRequiredNotificationReceivedEvent) =
  {report("verifyPermissionRequiredNotificationReceived, not implemented")}

  def selectVerifierRequestNotificationReceivedEvent(e: SelectVerifierRequestNotificationReceivedEvent) =
  {report("selectVerifierRequestNotificationReceivedEvent, not implemented")}

  def getClaimResponseReceived(e: GetClaimResponseReceivedEvent) =
  {report("getClaimResponseReceived, not implemented")}

  def verifyContentRequestNotificationReceived(e: VerifyContentRequestNotificationReceivedEvent) =
  {report("verifyContentRequestNotificationReceived, not implemented")}

  def verifyContentResponseReceived(e: VerifyContentResponseReceivedEvent) =
  {report("verifyContentResponseReceived, not implemented")}

  def createInvitationResponseReceived(e: CreateInvitationResponseReceivedEvent) =
  {report("createInvitationResponseReceived, not implemented")}

  def createIntroductionResponseReceived(e: CreateIntroductionResponseReceivedEvent) =
  {report("createIntroductionResponseReceived, not implemented")}

//  def createReferralResponseReceived(e: CreateReferralResponseReceivedEvent) =
//  {report("createReferralResponseReceived, not implemented")}

}
