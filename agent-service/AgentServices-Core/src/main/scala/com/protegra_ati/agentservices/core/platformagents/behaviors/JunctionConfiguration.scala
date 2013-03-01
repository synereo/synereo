package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import net.lag.configgy._
import java.net.URI
import com.protegra_ati.agentservices.core.util.rabbit.RabbitConfiguration

trait JunctionConfiguration
{
  def loadFirstURI(@transient configBlock: Option[ ConfigMap ]): URI =
  {
    loadURIs(configBlock).head
  }

  def loadURIs(@transient configBlock: Option[ ConfigMap ]): List[ URI ] =
  {
    var result: List[ URI ] = Nil;

    val URIKey = "URI"
    val portKey = "port"

    configBlock match {
      case Some(block) => {
        for ( key <- block.keys ) {
          block.getConfigMap(key) match {
            case Some(acquaintanceMap) => {
              val host = acquaintanceMap.getString(URIKey).getOrElse("")
              val portOpt = acquaintanceMap.getInt(portKey)
              val acquaintance = portOpt match {
                case Some(port) => host.toURI.withPort(port)
                case _ => host.toURI
              }
              result = List[ URI ](acquaintance) ::: result
            }
            case _ => {}
          }
        }
      }
      case _ => {}
    }
    result
  }

  def loadFirstRabbitConfig(@transient configBlock: Option[ ConfigMap ]): RabbitConfiguration =
  {
    loadRabbitConfigs(configBlock).head
  }

  def loadRabbitConfigs(@transient configBlock: Option[ ConfigMap ]): List[ RabbitConfiguration ] =
  {
    var result: List[ RabbitConfiguration ] = Nil;

    val URIKey = "URI"
    val portKey = "port"
    val userIdKey = "userId"
    val passwordKey = "password"
    configBlock match {
      case Some(block) => {
        for ( key <- block.keys ) {
          block.getConfigMap(key) match {
            case Some(acquaintanceMap) => {
              val host = acquaintanceMap.getString(URIKey).getOrElse("")
              val portOpt = acquaintanceMap.getInt(portKey)
              val userId =  acquaintanceMap.getString(userIdKey).getOrElse("guest")
              val password =  acquaintanceMap.getString(passwordKey).getOrElse("guest")
              val rabbitConfiguration = new RabbitConfiguration(host, portOpt.getOrElse(0), userId, password)
              result = List[ RabbitConfiguration ](rabbitConfiguration) ::: result
            }
            case _ => {}
          }
        }
      }
      case _ => {}
    }
    result
  }
}