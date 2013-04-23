package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents.BasePlatformAgent
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.protegra_ati.agentservices.core.util.rabbit.RabbitConfiguration
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.{Being, PersistedKVDBNodeRequest, PersistedKVDBNodeResponse}
import java.net.URI

trait Private {
  self: BasePlatformAgent =>

  var _privateLocation: URI = null
  var _privateAcquaintanceAddresses = List[URI]()
  var _privateQ: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] = null
  var _privateNetworkMode = "Rabbit"
  var _privateRabbitConfig: RabbitConfiguration = null

  def initPrivate(config: ConfigurationManager) {
    _privateLocation = config.privateSelf
    _privateAcquaintanceAddresses = config.privateAcquaintances ::: this._privateAcquaintanceAddresses
    _privateRabbitConfig = config.privateRabbit
    _privateNetworkMode = config.privateNetworkMode
  }

  def loadPrivateQueue() {
    if (isPrivateKVDBNetworkMode) {
      _privateQ = createNode(_privateLocation, _privateAcquaintanceAddresses)
    }
  }

  def isPrivateRabbitMQNetworkMode: Boolean = {
    _privateNetworkMode.toLowerCase == "rabbit"
  }

  def isPrivateKVDBNetworkMode: Boolean = {
    !isPrivateRabbitMQNetworkMode
  }
}
