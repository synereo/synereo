package com.ati.iaservices.helpers

import com.ati.iaservices.recipes.LauncherPluginUtil
import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent
import com.protegra_ati.agentservices.core.util.ConfigurationManager

class CreateUIHelper {
  final val UI_CONFIG = "init_ui.conf"
  final val LOG_KVDB_CONFIG = "log.conf"
  final val LOG_KVDB_PROPERTIES = "log.properties"
  final val LOG_AGENTSERVICES_CONFIG = "log_agentservices.conf"
  final val LOG_AGENTSERVICES_PROPERTIES = "log_agentservices.properties"

  //refactor this into core
  def checkAllStoreConfigFiles() {
    LauncherPluginUtil.configFileExists(UI_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_PROPERTIES)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_PROPERTIES)
  }

  def createUI(): AgentHostUIPlatformAgent = {
    val ui = new AgentHostUIPlatformAgent()
    checkAllStoreConfigFiles()
    val config = new ConfigurationManager(UI_CONFIG)
    ui.initFromConfig(config)
    ui
  }
}
