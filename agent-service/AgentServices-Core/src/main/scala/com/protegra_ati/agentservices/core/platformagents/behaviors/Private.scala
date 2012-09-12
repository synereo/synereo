package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS._
import com.biosimilarity.lift.lib.moniker._
import net.lag.configgy._

trait Private {
  self:BasePlatformAgent =>

  var _privateLocation: URM = null
  var _privateAcquaintanceAddresses = List[URM]()
  var _privateQ : PartitionedStringMGJ = null //persistedJunction

  def initPrivate(configUtil: Config)
  {
    val privateSelfMapKey = "private.self"
    _privateLocation = loadFirstURM(configUtil.getConfigMap(privateSelfMapKey))

    val privateAcquaintanceMapKey = "private.acquaintances"
    _privateAcquaintanceAddresses = loadURMs(configUtil.getConfigMap(privateAcquaintanceMapKey))  ::: this._privateAcquaintanceAddresses
  }

  def initPrivate(privateLocation: URM, privateAcquaintanceAddresses: List[ URM ])
  {
    _privateLocation = privateLocation
    _privateAcquaintanceAddresses = privateAcquaintanceAddresses ::: this._privateAcquaintanceAddresses
  }

  def loadPrivateQueue() = {
    _privateQ = new PartitionedStringMGJ(_privateLocation, _privateAcquaintanceAddresses, None)
  }
}