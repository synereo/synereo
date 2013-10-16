package com.protegra_ati.agentservices.msgs.agent.introduction

import com.biosimilarity.evaluator.distribution.ConcreteHL._
import java.net.URI

//# Message Set

trait AgentIntroduction

//## Methods on Aliases
//### Introduction Protocol
//#### beginIntroduction
case class beginIntroductionRequest(
  sessionURI : URI,
  alias : String,
  aBiCnxn : BiCnxn,
  bBiCnxn : BiCnxn,
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
