package com.ati.iaservices.recipes

import com.ati.iaservices.helpers.CreateStoreHelper
import com.ati.iaservices.recipes.LauncherPluginSession.session

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
class CreateStorePlugin extends LauncherPluginBase {
  val pluginName = "CreateStore"
  val exitOnFail = true

  override def validateSession() = {
  }

  override def execute(args: Array[ String ]): Unit = {
    createStore
  }

  def createStore(): Unit = {
    session.store = new CreateStoreHelper().createStore()
  }
}
