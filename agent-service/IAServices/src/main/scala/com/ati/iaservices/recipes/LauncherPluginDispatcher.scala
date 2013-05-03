package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse

class LauncherPluginDispatcher {
  def dispatch(args: Array[String]) {
    for (idx <- 0 until args.size) {
      val plugin = getPlugin(args(idx))
      plugin.run(args)
    }
  }

  def getPlugin(arg: String): LauncherPluginBase = arg match {
    case "store" => new CreateStorePlugin()
    case "dsl" => new CreateDSLPlugin()
    case "registerAgent" => new RegisterAgentPlugin() {
      def handleListen(response: RegistrationResponse) {

      }
    }
    case "connectToAll" => new ConnectToAllPlugin()
  }
}
