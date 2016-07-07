package com.biosimilarity.evaluator.distribution

object EvalConfConfig extends EvalConfig {

  val config = evalConfig()

  def read(prm : String ) : String = {
    try { config.getString(prm) }
    catch { case _ => throw new Exception("Missing or empty value for: " +prm + " in eval.conf file.") }
  }

  def read(prm : String, dflt : String ) : String = {
    try { config.getString(prm) }
    catch { case _ => dflt }
  }

  def readInt(prm : String ) : Int = {
    try { config.getInt(prm) }
    catch { case _ => throw new Exception("Missing or empty value for: " +prm + " in eval.conf file.") }
  }

  def readInt(prm : String, dflt : Int ) : Int = {
    try { config.getInt(prm) }
    catch { case _ => dflt }
  }

  def isOmniRequired() : Boolean = {
    read("OmniRPCURI", "miss") != "miss"
  }

}



