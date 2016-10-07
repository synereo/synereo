package com.biosimilarity.evaluator.distribution

trait DeploymentMode
case object Colocated extends DeploymentMode
case object Distributed extends DeploymentMode
