package com.protegra_ati.agentservices.core.util

import com.protegra_ati.agentservices.core.util.rabbit.RabbitConfiguration
import com.typesafe.config.ConfigFactory
import java.io.File
import java.net.URI
import java.util.UUID

class ConfigurationManager (fileName: String) {
  private val _config = ConfigFactory.load(ConfigFactory.parseFile(new File(fileName)))
  private val _prefix = "agentservices.core"

  def id: UUID = getUUID("id")
  def appBizNetworkId = getUUID("appBizNetworkId")
  def networkMode = getString("networkMode")
  def privateNetworkMode = getString("privateNetworkMode")
  def publicSelf = getURI("public.self")
  def publicAcquaintances = getURIList("public.acquaintances")
  def privateSelf = getURI("private.self")
  def privateAcquaintances = getURIList("private.acquaintances")
  def privateRabbit = getRabbitConfiguration("private.rabbit")
  def dbSelf = getURI("db.self")

  protected def getString(path: String): String = {
    _config.getString(_prefix + path)
  }

  protected def getStringOrElse(path: String, default: String): String = {
    if (_config.hasPath(_prefix + path)) {
      _config.getString(_prefix + path)
    } else {
      default
    }
  }

  protected def getInt(path: String): Int = {
    _config.getInt(_prefix + path)
  }

  protected def getIntOrElse(path: String, default: Int): Int = {
    if (_config.hasPath(_prefix + path)) {
      _config.getInt(_prefix + path)
    } else {
      default
    }
  }

  protected def getUUID(path: String): UUID = {
    UUID.fromString(_config.getString(_prefix + path))
  }

  protected def getURI(path: String): URI = {
    val host = getString(path + ".host")
    val port = getInt(path + ".port")
    new URI(null, null, host, port, null, null, null)
  }

  protected def getURIList(path: String): List[URI] = {
    //TODO
    Nil
  }

  protected def getRabbitConfiguration(path: String): RabbitConfiguration = {
    val host = getString(path + ".host")
    val port = getInt(path + ".port")
    val userId = getStringOrElse(path + ".userId", null)
    val password = getStringOrElse(path + ".password", null)
    new RabbitConfiguration(host, port, userId, password)
  }
}
