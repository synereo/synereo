package com.ati.iaservices.recipes

import com.protegra_ati.agentservices.store.util.LogConfiguration._

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 9:53 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class LauncherPluginBase {
  val pluginName: String
  val exitOnFail: Boolean

  def run() : Unit = {
    run(Array(""))
  }

  def run(args: Array[ String ]): Unit = {

    try {
      validateSession
      execute(args)
    }
    catch {
      case e:Exception => {
        logger.error("Exception returned from Plugin " + pluginName, e)
        if (exitOnFail) throw e
      }
    }
  }

  def validateSession()
  def execute(args: Array[ String ])
}
