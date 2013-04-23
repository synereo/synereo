package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents.BasePlatformAgent
import com.protegra_ati.agentservices.core.util.ConfigurationManager
import com.protegra_ati.agentservices.store.util.Severity
import java.util.UUID

trait Applications {
  self: BasePlatformAgent =>

  var _appBizNetworkId: UUID = null

  def initApps(config: ConfigurationManager) {
    try {
      this._appBizNetworkId = config.appBizNetworkId
    }
    catch {
      case e: Exception => report("failed to load appBizNetworkId from config", e, Severity.Fatal)
    }
  }
}
