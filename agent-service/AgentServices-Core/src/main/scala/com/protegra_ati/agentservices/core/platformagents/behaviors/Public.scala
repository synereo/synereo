package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS._
import com.biosimilarity.lift.lib.moniker._
import net.lag.configgy._

trait Public {
  self:BasePlatformAgent =>

  var _publicLocation: URM = null
  var _publicAcquaintanceAddresses = List[URM]()
  var _publicQ : PartitionedStringMGJ = null //persistedJunction

  def initPublic(configUtil: Config)
  {
    val publicSelfMapKey = "public.self"
    _publicLocation = loadFirstURM(configUtil.getConfigMap(publicSelfMapKey))

    val publicAcquaintanceMapKey = "public.acquaintances"
    _publicAcquaintanceAddresses = loadURMs(configUtil.getConfigMap(publicAcquaintanceMapKey))  ::: this._publicAcquaintanceAddresses
  }

  def initPublic(publicLocation: URM, publicAcquaintanceAddresses: List[ URM ])
  {
    _publicLocation = publicLocation
    _publicAcquaintanceAddresses = publicAcquaintanceAddresses ::: this._publicAcquaintanceAddresses
  }

  def loadPublicQueue() = {
    _publicQ = new PartitionedStringMGJ(_publicLocation, _publicAcquaintanceAddresses, None)
  }
}