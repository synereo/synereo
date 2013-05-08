package com.protegra_ati.agentservices.core.schema.disclosure

//Constants of enum style for default disclosed data trust levels
object TrustLevel extends Enumeration {
  type TrustLevel = Value

  val Trusted = Value("Trusted")
  val Custom = Value("Custom")
  val Basic = Value("Basic")
  val Empty = Value("Empty")
}
