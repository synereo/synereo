package com.ati.iaservices.helpers

import com.ati.iaservices.platformagents.AgentHostDslPlatformAgent
import com.ati.iaservices.recipes.LauncherPluginUtil
import com.protegra_ati.agentservices.core.util.ConfigurationManager

class CreateDslHelper {
  final val DSL_CONFIG = "init_ui.conf"
  final val LOG_KVDB_CONFIG = "log.conf"
  final val LOG_KVDB_PROPERTIES = "log.properties"
  final val LOG_AGENTSERVICES_CONFIG = "log_agentservices.conf"
  final val LOG_AGENTSERVICES_PROPERTIES = "log_agentservices.properties"

  def checkAllStoreConfigFiles() {
    LauncherPluginUtil.configFileExists(DSL_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_PROPERTIES)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_PROPERTIES)
  }

  def createDsl(): AgentHostDslPlatformAgent = {
    val dsl = new AgentHostDslPlatformAgent()
    checkAllStoreConfigFiles()
    val config = new ConfigurationManager(DSL_CONFIG)
    dsl.initFromConfig(config)
    dsl
  }
}
