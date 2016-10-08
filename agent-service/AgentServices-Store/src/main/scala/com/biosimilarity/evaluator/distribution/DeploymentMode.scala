package com.biosimilarity.evaluator.distribution

trait DeploymentMode
case object Colocated extends DeploymentMode {
  override def toString: String = "colocated"
}
case object Distributed extends DeploymentMode {
  override def toString: String = "distributed"
}
