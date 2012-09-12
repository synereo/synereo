package com.protegra_ati.agentservices.core.schema

import java.util.UUID
import reflect.BeanProperty
import com.protegra.agentservicesstore.AgentTS.acT._

/* User: jviolago
*/

case class Verifier(@BeanProperty var name:String) extends Data {
  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("")
}