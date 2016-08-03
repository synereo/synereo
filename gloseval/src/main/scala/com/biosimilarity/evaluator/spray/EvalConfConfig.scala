package com.biosimilarity.evaluator.distribution

import com.typesafe.config.Config

import scala.collection.JavaConversions._
import scala.util.Try

object EvalConfConfig extends EvalConfig {

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

  val serverPort    = EvalConfConfig.readIntOrElse("serverPort", 80)
  val serverSSLPort = EvalConfConfig.readIntOrElse("serverSSLPort", 443)
}
