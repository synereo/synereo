package com.ati.iaservices.recipes

import java.io.File
import com.protegra_ati.agentservices.core.schema.Profile
import scala.util.Random
import com.protegra_ati.agentservices.store.util.LogConfiguration._


class LauncherPluginUtil

object LauncherPluginUtil {
  def configFileExists(filePath: String) {
    val f = new File(filePath)
    if (!f.exists()) {
      val errorMessage = "Missing config file for : " + filePath
      logger.error(errorMessage)
      throw new Exception(errorMessage)
    }
  }
}
