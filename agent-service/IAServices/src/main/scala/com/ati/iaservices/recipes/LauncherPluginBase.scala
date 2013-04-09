package com.ati.iaservices.recipes

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
      println("Running Plugin: " + pluginName)

      validateSession
      execute(args)

      println("Plugin Completed: " + pluginName)
    }
    catch {
      case e:Exception => {
        println("Exception returned from Plugin " + pluginName + ": " + e.getMessage)
        if (exitOnFail) throw e
      }
    }
  }

  def validateSession()
  def execute(args: Array[ String ])
}
