package com.ati.iaservices.events

import com.protegra_ati.agentservices.store.util.Reporting

trait MessageEventListener extends Reporting
{
  def createReferralResponseReceived(e: CreateReferralResponseReceivedEvent) =
  {
    report("createReferralResponseReceived, not implemented")
  }
}
