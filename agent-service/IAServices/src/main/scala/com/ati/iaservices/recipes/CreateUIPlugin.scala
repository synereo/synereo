package com.ati.iaservices.recipes

import com.ati.iaservices.helpers.CreateUIHelper
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.store.util.LogConfiguration._

class CreateUIPlugin extends LauncherPluginBase {
  val pluginName = "CreateUI"
  val exitOnFail = true

  override def validateSession() {
  }

  override def execute(args: Array[String]) {
    createUI()
  }

  def createUI() {
    session.ui = new CreateUIHelper().createUI()
    logger.info("UIPlatformAgent launcher started")
  }
}
