package com.ati.iaservices.recipes

import com.ati.iaservices.helpers.CreateStoreHelper
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.store.util.LogConfiguration._

class CreateStorePlugin extends LauncherPluginBase {
  val pluginName = "CreateStore"
  val exitOnFail = true

  override def validateSession() {
  }

  override def execute(args: Array[String]) {
    createStore()
  }

  def createStore() {
    session.store = new CreateStoreHelper().createStore()
    logger.info("StorePlatformAgent launcher started")
  }
}
