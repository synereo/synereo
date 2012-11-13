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

  var _networkMode = "Distributed"

  def initPublic(configUtil: Config)
  {
    val publicSelfMapKey = "public.self"
    _publicLocation = loadFirstURM(configUtil.getConfigMap(publicSelfMapKey))

    val publicAcquaintanceMapKey = "public.acquaintances"
    _publicAcquaintanceAddresses = loadURMs(configUtil.getConfigMap(publicAcquaintanceMapKey))  ::: this._publicAcquaintanceAddresses

    val networkModeMapKey = "networkMode"
    this._networkMode = configUtil.getString(networkModeMapKey).getOrElse("Distributed")
    //System.err.println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   In initPublic _networkMode = " + _networkMode);

  }

  def initPublic(publicLocation: URM, publicAcquaintanceAddresses: List[ URM ])
  {
    _publicLocation = publicLocation
    _publicAcquaintanceAddresses = publicAcquaintanceAddresses ::: this._publicAcquaintanceAddresses
  }

  def loadPublicQueue() = {
    if (isDistributedNetworkMode)
      _publicQ = new PartitionedStringMGJ(_publicLocation, _publicAcquaintanceAddresses, None)
  }

  def isLocalNetworkMode() = {
    _networkMode == "Local"
  }

  def isDistributedNetworkMode() = {
    !isLocalNetworkMode
  }


}