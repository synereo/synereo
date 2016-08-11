package com.biosimilarity.evaluator.spray

import com.biosimilarity.lift.lib.ConfigurationTrampoline

object NodeUser extends ConfigurationTrampoline {
  //val imgFileName = "./src/main/resources/media/queenbee64.txt"
  //val imgSrc = io.Source.fromFile( imgFileName ).getLines.mkString
  val imgSrc = QueenBeeImage.imgBase64
  val bailMEmail = "Missing or empty email, update \"nodeAdminEmail\" in eval.conf file."
  val bailMPass =  "Missing or empty password, update \"nodeAdminPass\" in eval.conf file."
  val bailMName =  "Missing or empty name, update \"nodeAdminName\" in eval.conf file."
  val email = if (nodeAdminEmail.trim.isEmpty) bail(bailMEmail) else nodeAdminEmail //"splicious-design-team@googlegroups.com"
  val password = if (nodeAdminPass.trim.isEmpty) bail(bailMPass) else nodeAdminPass //"splicious"
  val name = if (nodeAdminName.trim.isEmpty) bail(bailMName) else nodeAdminName //"Queen Splicious"
  val jsonBlob = s"""{"name":"$name","imgSrc":"$imgSrc"}"""

  override def configFileName = { Option ("eval.conf") }
  override def configurationDefaults: ConfigurationDefaults = NodeAdminDefaults.asInstanceOf[ConfigurationDefaults]
  def bail (eMessage: String) : String = {
    throw new Exception( "Exception while creating node admin user. " + eMessage)
  }

  def nodeAdminEmail: String = { configurationFromFile.getOrElse("nodeAdminEmail",bail(bailMEmail))}
  def nodeAdminPass: String = { configurationFromFile.getOrElse("nodeAdminPass",bail(bailMPass))}
  def nodeAdminName: String = { configurationFromFile.getOrElse("nodeAdminName",bail(bailMName))}
}

object NodeAdminDefaults extends Serializable {
  val nodeAdminEmail : String = ""//"admin@"+InetAddress.getLocalHost.getHostName.toLowerCase
  val nodeAdminPass = ""//""password"
  val nodeAdminName : String = ""//"Node Admin"
}

object QueenBeeImage {
  var imgBase64="data:image/png;base64,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"
}
