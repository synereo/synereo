package com.biosimilarity.evaluator.spray.agent

import com.biosimilarity.evaluator.spray.util.Slot._
import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store._

trait AgentT {
  def aliases: List[AliasT]
  def defaultAlias: AliasT
  def externalIdentities: List[AliasT]
}
