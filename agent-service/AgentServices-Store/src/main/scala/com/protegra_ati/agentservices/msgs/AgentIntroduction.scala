package com.protegra_ati.agentservices.msgs.agent.introduction

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import java.net.URI

//# Message Set

trait AgentIntroduction

//## Methods on Aliases
//### Introduction Protocol
//#### beginIntroduction
case class beginIntroductionRequest(
  sessionURI : URI,
  alias : String,
  aConnection : PortableAgentCnxn,
  bConnection : PortableAgentCnxn,
  aMessage : String,
  bMessage : String
) extends AgentIntroduction
case class beginIntroductionError(
  sessionURI : URI,
  reason : String
) extends AgentIntroduction
case class beginIntroductionResponse(
  sessionURI : URI
) extends AgentIntroduction

//#### introductionConfirmation
case class introductionConfirmationRequest(
  sessionURI : URI,
  alias : String,
  introSessionId : String,
  correlationId : String,
  accepted : Boolean
) extends AgentIntroduction
case class introductionConfirmationError(
  sessionURI : URI,
  reason : String
) extends AgentIntroduction
case class introductionConfirmationResponse(
  sessionURI : URI
) extends AgentIntroduction
