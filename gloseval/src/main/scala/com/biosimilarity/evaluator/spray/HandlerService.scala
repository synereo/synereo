// -*- mode: Scala;-*-
// Filename:    HandlerService.scala
// Authors:     lgm
// Creation:    Wed Oct  2 17:51:11 2013
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution.{AccordionConfiguration, DSLCommLinkConfiguration, EvalConfig, EvaluationCommsService}

@SerialVersionUID(1000L)
object EvalHandlerService
    extends EvalHandler
    with EvaluationCommsService
    with EvalConfig
    with DSLCommLinkConfiguration
    with AccordionConfiguration
    with DownStreamHttpCommsT
    with BTCHandler
    with Serializable

object EvalAndAgentCRUDHandlerService
    extends EvalHandler
    with AgentCRUDHandler
    with EvaluationCommsService
    with EvalConfig
    with DSLCommLinkConfiguration
    with AccordionConfiguration
    with DownStreamHttpCommsT
    with BTCHandler
    with Serializable
