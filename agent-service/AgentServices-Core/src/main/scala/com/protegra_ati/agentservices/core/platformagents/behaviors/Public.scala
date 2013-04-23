package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents.BasePlatformAgent
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.{Being, PersistedKVDBNodeRequest, PersistedKVDBNodeResponse}
import java.net.URI

trait Public {
  self: BasePlatformAgent =>

  var _publicLocation: URI = null
  var _publicAcquaintanceAddresses = List[URI]()
  var _publicQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = null
  var _networkMode = "Distributed"

  def initPublic(config: ConfigurationManager) {
    _publicLocation = config.publicSelf
    _publicAcquaintanceAddresses = config.publicAcquaintances ::: this._publicAcquaintanceAddresses
    _networkMode = config.networkMode
  }

  def loadPublicQueue() {
    _publicQ = createNode(_publicLocation, _publicAcquaintanceAddresses)
  }

  def isLocalNetworkMode: Boolean = {
    _networkMode.toLowerCase == "local"
  }

  def isDistributedNetworkMode: Boolean = {
    !isLocalNetworkMode
  }
}
