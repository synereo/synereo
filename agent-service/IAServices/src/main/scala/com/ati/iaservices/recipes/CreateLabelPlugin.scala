package com.ati.iaservices.recipes

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 4:40 PM
 * To change this template use File | Settings | File Templates.
 */
class CreateLabelPlugin extends LauncherPluginBase {
  val pluginName = "CreateLabel"
  val exitOnFail = true

  override def validateSession(): Unit = {
    // no additional initialization required
    if (LauncherPluginSession.session.ui == null)
      throw new Exception("session.ui has not been initialized.")
  }

  override def execute(args: Array[ String ]): Unit = {
    createLabel
  }

  def createLabel(): Unit = {

  }
}
