package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import net.lag.configgy._
import java.net.URI

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
}