package com.biosimilarity.evaluator.distribution

import com.typesafe.config.Config

import scala.util.Try

object EvalConfConfig extends EvalConfig {

  val config: Config = evalConfig()

  def read(param: String): String =
    try {
      config.getString(param)
    } catch {
      case _: Throwable => throw new IllegalStateException(s"Missing or empty value for: $param in eval.conf file.")
    }

  def read(param: String, default: String): String =
    try {
      config.getString(param)
    } catch {
      case _: Throwable => default
    }

  def readInt(param: String): Int =
    try {
      config.getInt(param)
    } catch {
      case _: Throwable => throw new IllegalStateException(s"Missing or empty value for: $param in eval.conf file.")
    }

  def readInt(param: String, default: Int): Int =
    try {
      config.getInt(param)
    } catch {
      case _: Throwable => default
    }

  def isOmniRequired: Boolean = Try(read("OmniRPCURI")).isSuccess
}
