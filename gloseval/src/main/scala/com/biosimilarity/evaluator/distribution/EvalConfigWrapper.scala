package com.biosimilarity.evaluator.distribution

import java.io.File

import scala.collection.JavaConversions._
import scala.util.Try

object EvalConfigWrapper extends EvalConfig with Serializable {

  def readString(param: String): String = evalConfig().getString(param)

  def readStringOrElse(param: String, default: String): String = Try(readString(param)).getOrElse(default)

  def readInt(param: String): Int = evalConfig().getInt(param)

  def readIntOrElse(param: String, default: Int): Int = Try(readInt(param)).getOrElse(default)

  def readList(param: String): List[String] = evalConfig().getStringList(param).toList

  def isOmniRequired(): Boolean = Try(readString("OmniRPCURI")).isSuccess

  def serverPort    = readIntOrElse("serverPort", 80)

  def serverSSLPort = readIntOrElse("serverSSLPort", 443)

  def serviceDemoDataFile: File = new File(readString("ImporterServiceDemoDataFile"))

  def serviceHost: String = readString("ImporterServiceHost")

  def servicePort: Int = readInt("ImporterServicePort")

  def serviceHostURI: String = "https://%s:%d/api".format(serviceHost, servicePort)

  def nodeMode(): NodeMode =
    evalConfig().getString("mode") match {
      case "headless" => Headless
      case "headed" => Headed
    }
}
