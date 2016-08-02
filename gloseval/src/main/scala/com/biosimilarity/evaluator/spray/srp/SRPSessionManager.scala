package com.biosimilarity.evaluator.spray.srp

object SRPSessionManager {
  var loginSession:Map[String, UserCredentials] = Map()

  def saveSession(key: String, identity: UserCredentials):Unit = loginSession += (key -> identity)

  def getSessionWithHash(key: String): UserCredentials =
    loginSession.getOrElse(key, loginSession.getOrElse(s"00$key", null))

}

