package com.biosimilarity.evaluator.spray.agent

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

trait AliasT {
  def agent: AgentT
  def labels: List[CnxnCtxtLabel[String,String,String]]
  def externalIdentities: List[AliasT]
}
