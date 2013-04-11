package com.ati.iaservices.helpers

import com.protegra_ati.agentservices.core.platformagents.AgentHostStorePlatformAgent
import com.ati.iaservices.recipes.LauncherPluginUtil

class CreateStoreHelper {
  final val STORE_CONFIG = "init_store.conf"
  final val LOG_KVDB_CONFIG = "log.conf"
  final val LOG_KVDB_PROPERTIES = "log.properties"
  final val LOG_AGENTSERVICES_CONFIG = "log_agentservices.conf"
  final val LOG_AGENTSERVICES_PROPERTIES = "log_agentservices.properties"

  //refactor this into core
  def checkAllStoreConfigFiles() = {
    LauncherPluginUtil.configFileExists(STORE_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_PROPERTIES)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_PROPERTIES)
  }

  def createStore(): AgentHostStorePlatformAgent = {
    val store = new AgentHostStorePlatformAgent()
    checkAllStoreConfigFiles
    store.initFromConfig(STORE_CONFIG)
    println("*************** StorePlatformAgent launcher started ***************")
    return store
  }

}
