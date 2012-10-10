package com.protegra_ati.agentservices.core.schema.util

import java.io.Serializable
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.schema.KVDBSerializable
import com.protegra_ati.agentservices.core.schema.Data
import scala.reflect.BeanProperty

/* User: mgevantmakher
*/

case class MockSystemData[ T <: Data ](@BeanProperty val data: T) extends Serializable with
KVDBSerializable
{
  def this() = this(null.asInstanceOf[ T ])

  //private val serialVersionUID = 7526471155622776147L;


}
