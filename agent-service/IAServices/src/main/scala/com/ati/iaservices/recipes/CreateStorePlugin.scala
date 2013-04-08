package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.platformagents.{AgentHostStorePlatformAgent, AgentHostUIPlatformAgent}
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
class CreateStorePlugin extends LauncherPluginBase {
  val pluginName = "CreateStore"
  val exitOnFail = true

  final val STORE_CONFIG = "init_store.conf"
  final val DB_STORE_CONFIG = "db_store.conf"
  final val DB_STORE_DB_CONFIG = "db_store_db.conf"
  final val DB_STORE_PUBLIC_CONFIG = "db_store_public.conf"
  final val LOG_KVDB_CONFIG = "log.conf"
  final val LOG_KVDB_PROPERTIES = "log.properties"
  final val LOG_AGENTSERVICES_CONFIG = "log_agentservices.conf"
  final val LOG_AGENTSERVICES_PROPERTIES = "log_agentservices.properties"

  //refactor this into core
  def checkAllStoreConfigFiles() = {
    LauncherPluginUtil.configFileExists(STORE_CONFIG)
    LauncherPluginUtil.configFileExists(DB_STORE_CONFIG)
    LauncherPluginUtil.configFileExists(DB_STORE_DB_CONFIG)
    LauncherPluginUtil.configFileExists(DB_STORE_PUBLIC_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_KVDB_PROPERTIES)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_CONFIG)
    LauncherPluginUtil.configFileExists(LOG_AGENTSERVICES_PROPERTIES)
  }

  override def validateSession() = {
  }

  override def execute(args: Array[ String ]): Unit = {
    createStore
  }

  def createStore(): Unit = {
    LauncherPluginSession.session.store = new AgentHostStorePlatformAgent()
    checkAllStoreConfigFiles()
    LauncherPluginSession.session.store.initFromConfig(STORE_CONFIG)
    println("*************** StorePlatformAgent launcher started ***************")
  }

}
