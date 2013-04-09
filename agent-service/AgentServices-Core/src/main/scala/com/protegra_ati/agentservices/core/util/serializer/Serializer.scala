package com.protegra_ati.agentservices.core.util.serializer

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import com.protegra_ati.agentservices.store.util._
import java.io._

object Serializer extends Reporting
{
  private var kryoSerializerReference = KryoSerializer.getInstance()

  def setKryoSerializerReference(serializerReference: KryoSerializer)
  {
    kryoSerializerReference = serializerReference
  }

  /**
   * serializes objects using different strategies into  ( base 64 encoded) string
   * @param obj  to be serialized
   * @tparam T  type
   * @return base 64 encoded string  or null in case of error or an null object
   */
  def serialize[ T ](obj: T): String =
  {
    serialize[ T ](obj, false) //true
  }

  private def serialize[ T ](obj: T, debug: Boolean): String =
  {
    if ( obj == null ) {
      report("NULL -object can't be serialized", Severity.Info)
      return null
    } else {
      return kryoSerializerReference.serialize(obj)
    }
  }

  def deserialize[ T ](source: String): T =
  {
    if ( source == null ) {
     // report("NULL string can't be deserialized to an object", Severity.Error)
      return null.asInstanceOf[ T ]
    } else {
      return kryoSerializerReference.deserialize(source)
    }
  }

}
