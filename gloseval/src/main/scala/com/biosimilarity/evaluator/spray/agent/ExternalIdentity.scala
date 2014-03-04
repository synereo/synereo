package com.biosimilarity.evaluator.spray.agent

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

trait ExternalIdentityT {}

case class EmailIdentity(address: String) extends ExternalIdentityT
