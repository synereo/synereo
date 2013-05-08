package com.ati.iaservices.recipes

import com.ati.iaservices.helpers.CreateDSLHelper
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.store.util.LogConfiguration._

class CreateDSLPlugin extends LauncherPluginBase {
  val pluginName = "CreateDSL"
  val exitOnFail = true

  override def validateSession() {
  }

  override def execute(args: Array[String]) {
    createDSL()
  }

  def createDSL() {
    session.dsl = new CreateDSLHelper().createDSL()
    logger.info("DSLPlatformAgent launcher started")
  }
}
