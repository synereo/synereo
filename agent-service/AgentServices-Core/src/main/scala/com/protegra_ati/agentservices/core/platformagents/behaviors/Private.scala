package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import java.net.URI
import net.lag.configgy._
import com.protegra_ati.agentservices.core.util.rabbit.RabbitConfiguration

trait Private {
  self:BasePlatformAgent =>

  var _privateLocation: URI = null
  var _privateAcquaintanceAddresses = List[URI]()
  var _privateRabbitLocation: URI = null
  var _privateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] = null //persistedJunction
  var _privateConfigFileName: Option[String] = None

  var _privateNetworkMode = "Rabbit"
  var _privateRabbitConfig: RabbitConfiguration = null

  def initPrivate(@transient configUtil: Config, privateConfigFileName: Option[String])
  {
    val privateSelfMapKey = "private.self"
    _privateLocation = loadFirstURI(configUtil.getConfigMap(privateSelfMapKey))

    val privateAcquaintanceMapKey = "private.acquaintances"
    _privateAcquaintanceAddresses = loadURIs(configUtil.getConfigMap(privateAcquaintanceMapKey))  ::: this._privateAcquaintanceAddresses

    val privateRabbitMapKey = "private.rabbit"
    _privateRabbitLocation = loadFirstURI(configUtil.getConfigMap(privateRabbitMapKey))

    val privateNetworkModeMapKey = "privateNetworkMode"
    _privateNetworkMode = configUtil.getString(privateNetworkModeMapKey).getOrElse("Rabbit")
    _privateConfigFileName = privateConfigFileName

  }
//
//  def initPrivate(privateLocation: URI, privateAcquaintanceAddresses: List[ URI ], privateRabbitLocation: URI, privateConfigFileName: Option[String])
//  {
//    _privateLocation = privateLocation
//    _privateAcquaintanceAddresses = privateAcquaintanceAddresses ::: this._privateAcquaintanceAddresses
//    _privateRabbitLocation = privateRabbitLocation
//    _privateConfigFileName = privateConfigFileName
//  }

  def loadPrivateQueue() = {
    _privateRabbitConfig = new RabbitConfiguration(_privateRabbitLocation.getHost, _privateRabbitLocation.getPort)
    if (isPrivateKVDBNetworkMode)
      _privateQ = createNode(_privateLocation, _privateAcquaintanceAddresses, _privateConfigFileName)
  }


  def isPrivateRabbitMQNetworkMode() =
  {
    _privateNetworkMode.toLowerCase == "Rabbit".toLowerCase
  }

  def isPrivateKVDBNetworkMode() =
  {
    !isPrivateRabbitMQNetworkMode
  }

  def setPrivateNetworkMode(mode: String) =
  {
    _privateNetworkMode = mode
  }

}