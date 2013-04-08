package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.platformagents.AgentHostUIPlatformAgent

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

  final val UI_CONFIG = "init_ui.conf"

  override def validateSession() = {
  }

  override def execute(args: Array[ String ]): Unit = {

    createUI
  }

  def createUI(): Unit = {
    LauncherPluginSession.session.ui = new AgentHostUIPlatformAgent()
    LauncherPluginUtil.configFileExists(UI_CONFIG)
    LauncherPluginSession.session.ui.initFromConfig(UI_CONFIG)
    println("*************** UIPlatformAgent launcher started ***************")
  }
}
