package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 1:08 PM
 * To change this template use File | Settings | File Templates.
 */
class LauncherPluginDispatcher {
  def dispatch(args: Array[String]) {
    for (idx <- 0 until args.size) {
      val plugin = getPlugin(args(idx))
      plugin.run(args)
    }
  }

  def getPlugin(arg:String): LauncherPluginBase = arg match {
    case "store" => new CreateStorePlugin()
    case "ui" => new CreateUIPlugin()
    case "registerAgent" => new RegisterAgentPlugin() {
      def handleListen(response: RegistrationResponse) = {

      }
    }
    case "connectToAll" => new ConnectToAllPlugin()
  }
}
