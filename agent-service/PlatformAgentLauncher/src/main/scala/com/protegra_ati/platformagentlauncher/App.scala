package com.protegra_ati.platformagentlauncher

import com.ati.iaservices.recipes._

//run me with "mvn scala:run"
object App {

  def main(args: Array[String]) {
    val launcher = new LauncherPluginDispatcher()

    launcher.dispatch(args)
  }
}
