package com.ati.iaservices.recipes

import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: pspratt
 * Date: 08/04/13
 * Time: 1:34 PM
 * To change this template use File | Settings | File Templates.
 */
class LauncherPluginUtil {

}

object LauncherPluginUtil {
  def configFileExists(filePath: String) =
  {
    val f = new File(filePath)
    if ( !f.exists() )
    {
      val errorMessage = "Missing config file for : " + filePath
      println(errorMessage)
      throw new Exception(errorMessage)
    }
  }
}
