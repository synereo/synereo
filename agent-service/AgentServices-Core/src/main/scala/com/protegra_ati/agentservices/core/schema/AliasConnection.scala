package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.core.schema.validator.AliasConnectionValidator

/* User: jviolago
*/

case class AliasConnection(alias:String, connectionId:String) extends Data
with AliasConnectionValidator{
  def this() = this("", "")
}