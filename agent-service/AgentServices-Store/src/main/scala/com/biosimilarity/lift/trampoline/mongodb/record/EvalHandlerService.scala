package com.biosimilarity.lift.trampoline.mongodb.record

import com.biosimilarity.evaluator.distribution.{DSLCommLinkConfiguration, EvalConfig, EvaluationCommsService}

object EvalHandlerService
  extends EvaluationCommsService
    with EvalConfig
    with DSLCommLinkConfiguration
    with Serializable
