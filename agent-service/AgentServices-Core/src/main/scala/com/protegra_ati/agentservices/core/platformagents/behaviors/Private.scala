package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import java.net.URI
import net.lag.configgy._

trait Private {
  self:BasePlatformAgent =>

  var _privateLocation: URI = null
  var _privateAcquaintanceAddresses = List[URI]()
  var _privateQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] = null //persistedJunction
  var _privateConfigFileName: Option[String] = None
    
  def initPrivate(@transient configUtil: Config)
  {
    val privateSelfMapKey = "private.self"
    _privateLocation = loadFirstURI(configUtil.getConfigMap(privateSelfMapKey))

    val privateAcquaintanceMapKey = "private.acquaintances"
    _privateAcquaintanceAddresses = loadURIs(configUtil.getConfigMap(privateAcquaintanceMapKey))  ::: this._privateAcquaintanceAddresses
  }

  def initPrivate(privateLocation: URI, privateAcquaintanceAddresses: List[ URI ], privateConfigFileName: Option[String])
  {
    _privateLocation = privateLocation
    _privateAcquaintanceAddresses = privateAcquaintanceAddresses ::: this._privateAcquaintanceAddresses
    _privateConfigFileName = privateConfigFileName
  }

  def loadPrivateQueue() = {
    _privateQ = createNode(_privateLocation, _privateAcquaintanceAddresses, _privateConfigFileName)
  }
}