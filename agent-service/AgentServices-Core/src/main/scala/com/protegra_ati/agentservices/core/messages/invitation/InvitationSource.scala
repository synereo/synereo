package com.protegra_ati.agentservices.core.messages.invitation

object InvitationSource extends Enumeration {
  type InvitationSource = Value

  //the source that an invitationRequest came from - useful to filter and audit in the UI
  val Search = Value("Search")
  val Introduction = Value("Introduction")
  val Referral = Value("Referral")
}
