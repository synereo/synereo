package com.biosimilarity.evaluator.distribution

import com.typesafe.config.{Config, ConfigFactory}

trait EvalConfig {

  var _config: Option[Config] = None

  def evalConfig(configFileName: String = "eval.conf"): Config = _config match {
    case Some(cfg) =>
      cfg
    case None =>
      val cfg = ConfigFactory.load(ConfigFactory.parseFile(new java.io.File(configFileName)) )
      _config = Some(cfg)
      cfg
  }

  def deploymentMode(): DeploymentMode =
    try {
      evalConfig().getString("deploymentMode") match {
        case "colocated" => Colocated
        case "distributed" => Distributed
      }
    } catch {
      case e: Throwable => Distributed
    }
}
