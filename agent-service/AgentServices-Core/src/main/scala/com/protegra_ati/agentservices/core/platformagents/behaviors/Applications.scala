package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import java.net.URI
import net.lag.configgy._
import java.util.UUID

//later support many apps on a single PA
trait Applications
{
  self: BasePlatformAgent =>

  var _appBizNetworkId: UUID = null

  def initApps(@transient configUtil: Config)
  {
    try {
      val privateAppBizNetworkMapKey = "appBizNetworkId"
      this._appBizNetworkId = UUID.fromString(configUtil.getString(privateAppBizNetworkMapKey).getOrElse(""))
    }
    catch {
      case e: Exception => report("failed to load appBizNetworkId from config")
    }
  }
}