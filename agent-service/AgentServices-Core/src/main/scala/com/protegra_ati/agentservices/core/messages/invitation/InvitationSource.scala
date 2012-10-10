package com.protegra_ati.agentservices.core.messages.invitation

object InvitationSource extends Enumeration("Search", "Introduction", "Referral")
{
  type InvitationSource = Value

  //the source that an invitationRequest came from - useful to filter and audit in the UI
  val Search, Introduction, Referral = Value
}
