package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import java.net.URI
import net.lag.configgy._

trait Public {
  self:BasePlatformAgent =>

  var _publicLocation: URI = null
  var _publicAcquaintanceAddresses = List[URI]()
  var _publicQ : Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] = null //persistedJunction

  def initPublic(configUtil: Config)
  {
    val publicSelfMapKey = "public.self"
    _publicLocation = loadFirstURI(configUtil.getConfigMap(publicSelfMapKey))

    val publicAcquaintanceMapKey = "public.acquaintances"
    _publicAcquaintanceAddresses = loadURIs(configUtil.getConfigMap(publicAcquaintanceMapKey))  ::: this._publicAcquaintanceAddresses
  }

  def initPublic(publicLocation: URI, publicAcquaintanceAddresses: List[ URI ])
  {
    _publicLocation = publicLocation
    _publicAcquaintanceAddresses = publicAcquaintanceAddresses ::: this._publicAcquaintanceAddresses
  }

  def loadPublicQueue() = {
    _publicQ = createNode(_publicLocation, _publicAcquaintanceAddresses)
  }
}