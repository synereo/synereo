package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import net.lag.configgy._
import com.biosimilarity.lift.lib.moniker._

trait JunctionConfiguration
{
  def loadFirstURM(configBlock: Option[ ConfigMap ]): URM =
  {
    loadURMs(configBlock).head
  }

  def loadURMs(configBlock: Option[ ConfigMap ]): List[ URM ] =
  {
    var result: List[ URM ] = Nil;

    val URMKey = "URI"
    val portKey = "port"

    configBlock match {
      case Some(block) => {
        for ( key <- block.keys ) {
          block.getConfigMap(key) match {
            case Some(acquaintanceMap) => {
              val host = acquaintanceMap.getString(URMKey).getOrElse("")
              val portOpt = acquaintanceMap.getInt(portKey)
              val acquaintance = portOpt match {
                case Some(port) => host.toURM.withPort(port)
                case _ => host.toURM
              }
              result = List[ URM ](acquaintance) ::: result
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