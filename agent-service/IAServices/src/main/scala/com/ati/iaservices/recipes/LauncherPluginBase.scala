package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.store.util.LogConfiguration._

abstract class LauncherPluginBase {
  val pluginName: String
  val exitOnFail: Boolean

  def run() {
    run(Array(""))
  }

  def run(args: Array[String]) {

    try {
      validateSession()
      execute(args)
    }
    catch {
      case e: Exception => {
        logger.error("Exception returned from Plugin " + pluginName, e)
        if (exitOnFail) throw e
      }
    }
  }

  def validateSession()

  def execute(args: Array[String])
}
