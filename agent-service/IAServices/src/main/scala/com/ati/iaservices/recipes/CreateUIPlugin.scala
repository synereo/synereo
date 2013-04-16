package com.ati.iaservices.recipes

import com.ati.iaservices.helpers.CreateUIHelper
import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.store.util.LogConfiguration._

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 */

class CreateUIPlugin extends LauncherPluginBase {
  val pluginName = "CreateUI"
  val exitOnFail = true

  override def validateSession() = {
  }

  override def execute(args: Array[ String ]): Unit = {
    createUI
  }

  def createUI(): Unit = {
    session.ui = new CreateUIHelper().createUI()
    logger.info("UIPlatformAgent launcher started")
  }
}
