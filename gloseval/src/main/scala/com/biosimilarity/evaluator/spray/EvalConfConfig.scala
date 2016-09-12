package com.biosimilarity.evaluator.distribution

import java.io.File

import com.typesafe.config.Config

import scala.collection.JavaConversions._
import scala.util.Try

object EvalConfConfig extends EvalConfig {

  sealed trait DeploymentMode
  case object Colocated   extends DeploymentMode
  case object Distributed extends DeploymentMode

  val config: Config = evalConfig()

  def readString(param: String): String =
    try {
      config.getString(param)
    } catch {
      case _: Throwable => throw new IllegalStateException(s"Missing or empty value for: $param in eval.conf file.")
    }

  def readStringOrElse(param: String, default: String): String =
    Try(readString(param)).getOrElse(default)

  def readInt(param: String): Int =
    try {
      config.getInt(param)
    } catch {
      case _: Throwable => throw new IllegalStateException(s"Missing or empty value for: $param in eval.conf file.")
    }

  def readIntOrElse(param: String, default: Int): Int =
    Try(readInt(param)).getOrElse(default)

  def readList(param: String): List[String] =
    try {
      config.getStringList(param).toList
    } catch {
      case _: Throwable => throw new Exception(s"Missing or empty value for: $param in eval.conf file.")
    }

  def isOmniRequired(): Boolean = Try(readString("OmniRPCURI")).isSuccess

  val serverPort    = readIntOrElse("serverPort", 80)
  val serverSSLPort = readIntOrElse("serverSSLPort", 443)

  def deploymentMode: DeploymentMode =
    readString("deploymentMode") match {
      case "distributed" => Distributed
      case _             => Colocated
    }

  def serviceDemoDataFile: File = new File(readString("ImporterServiceDemoDataFile"))

  def serviceHost: String = readString("ImporterServiceHost")

  def servicePort: Int = readInt("ImporterServicePort")

  def serviceHostURI: String = "https://%s:%d/api".format(serviceHost, servicePort)
}
