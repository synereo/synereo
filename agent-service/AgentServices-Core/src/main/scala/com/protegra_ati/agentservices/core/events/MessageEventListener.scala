/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.events

trait MessageEventListener
{
  def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
  {
    println("registrationResponseReceived, not implemented")
  }
//  def approveRegistrationResponseReceived(e: ApproveRegistrationResponseReceivedEvent) =
//  {
//    println("approveRegistrationResponseReceived, not implemented")
//  }
//  def affiliateResponseReceived(e: AffiliateResponseReceivedEvent) =
//  {
//    println("affiliateResponseReceived, not implemented")
//  }


  def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
  {
    println("in base GetContentResponseReceived, not implemented")
  }

//  def searchResponseReceived(e: SearchResponseReceivedEvent) =
//  {
//    println("in base searchResponseReceived, not implemented")
//  }

  def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
  {
    println("in base setContentResponseReceived, not implemented")
  }

  def setContentAdminResponseReceived(e: SetContentAdminResponseReceivedEvent) =
   {
     println("in base setContentAdminResponseReceived, not implemented")
   }
  def setContentPersistedResponseReceived(e: SetContentPersistedResponseReceivedEvent) =
  {
    println("in base setContentPersistedResponseReceived, not implemented")
  }

  def setContentAuthorizationResponseReceived(e: SetContentAuthorizationResponseReceivedEvent) =
  {
    println("in base setContentAuthorizationResponseReceived, not implemented")
  }

  def deleteContentResponseReceived(e: DeleteContentResponseReceivedEvent) =
  {
    println("in base deleteContentResponseReceived, not implemented")
  }

  def verifyResponseReceived(e: VerifyResponseReceivedEvent) =
  {
    println("in base verifyResponseReceived, not implemented")
  }

  def setLoginResponseReceived(e: SetLoginResponseReceivedEvent) =
  {println("setLoginResponseReceived, not implemented")}

  def getContentAuthorizationRequiredNotificationReceived(e: GetContentAuthorizationRequiredNotificationReceivedEvent) =
  {println("getContentAuthorizationRequiredNotificationReceived, not implemented")}

  def verifyPermissionRequiredNotificationReceived(e: VerifyPermissionRequiredNotificationReceivedEvent) =
  {println("verifyPermissionRequiredNotificationReceived, not implemented")}

  def selectVerifierRequestNotificationReceivedEvent(e: SelectVerifierRequestNotificationReceivedEvent) =
  {println("selectVerifierRequestNotificationReceivedEvent, not implemented")}

  def getClaimResponseReceived(e: GetClaimResponseReceivedEvent) =
  {println("getClaimResponseReceived, not implemented")}

  def verifyContentRequestNotificationReceived(e: VerifyContentRequestNotificationReceivedEvent) =
  {println("verifyContentRequestNotificationReceived, not implemented")}

  def verifyContentResponseReceived(e: VerifyContentResponseReceivedEvent) =
  {println("verifyContentResponseReceived, not implemented")}

  def createInvitationResponseReceived(e: CreateInvitationResponseReceivedEvent) =
  {println("createInvitationResponseReceived, not implemented")}

  def createIntroductionResponseReceived(e: CreateIntroductionResponseReceivedEvent) =
  {println("createIntroductionResponseReceived, not implemented")}

//  def createReferralResponseReceived(e: CreateReferralResponseReceivedEvent) =
//  {println("createReferralResponseReceived, not implemented")}

}
