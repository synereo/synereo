package com.protegra_ati.agentservices.core.util.serializer

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import com.protegra_ati.agentservices.store.util._
import java.io._

// TODO eventually Reporting has to be activation again
object Serializer// extends Reporting
{
  private var kryoSerializerReference = KryoSerializer.getInstance()
  private var javaIOSerializerReference = JavaIOSerializer.getInstance()

  def setKryoSerializerReference(serializerReference: KryoSerializer)
  {
    kryoSerializerReference = serializerReference
  }
  def setJavaIOSerializerReference(serializerReference: JavaIOSerializer)
  {
    javaIOSerializerReference = serializerReference
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
     // report("NULL -object can't be serialized", Severity.Error)
      return null
    } else {

      //TODO:re-enable when possible
      if ( obj.isInstanceOf[ UseJavaIOSerialization ] && !( obj.asInstanceOf[ UseJavaIOSerialization ].isJavaIOSerializationDeprecated ) ) {
        //        if ( debug )
        //          debugSerializer[ T ](obj)
        return javaIOSerializerReference.serialize(obj)
      } else if ( obj.isInstanceOf[ UseKryoSerialization ] && !( obj.asInstanceOf[ UseKryoSerialization ].isKryoSerializationDeprecated ) ) {
        //
        //        if ( debug )
        //          debugSerializer[ T ](obj)
        return kryoSerializerReference.serialize(obj)
      }
      // rest should be java io serialized
      else {
                //report("object " + obj + " does extend neither UseJavaIOSerialization nor UseKryoSerialization trait", Severity.Warning)

        try {

          //          if ( debug )
          //            debugSerializer[ T ](obj)

          return javaIOSerializerReference.serialize(obj)

        } catch {
          case ex: NotSerializableException => {
            //report("object " + obj + " can't be serialized " + ex.getStackTraceString, Severity.Error)
          }
          case ex: InvalidClassException => {
            //report("object " + obj + " can't be serialized " + ex.getStackTraceString, Severity.Error)
          }
        }
      }
    }
    return null // not reachable
  }


  def deserialize[ T ](source: String): T =
  {
    if ( source == null ) {
     // report("NULL string can't be deserialized to an object", Severity.Error)
      return null.asInstanceOf[ T ]
    } else {

      //TODO:re-enable when possible
      if ( source.startsWith(KryoSerializer.getInstance().getHeader) )
        return kryoSerializerReference.deserialize(source)
      // everything else deserialized using javaIO serializer
      else return javaIOSerializerReference.deserialize(source)


    }
    return ( null.asInstanceOf[ T ] )
  }


  def debugSerializer[ T ](obj: T)
  {
    println("DEBUG_SERIALIZATION")
    val serialized = serialize[ T ](obj, false)
    val deserialized = deserialize[ T ](serialized)
    if ( serialized == null || deserialized == null ) {
      println("ERROR: serialized=" + serialized + ", desedialized=" + deserialized)

      if ( !serialized.equals(deserialized) ) {
        println("ERROR: serialized=" + serialized + ", desedialized=" + deserialized)
      }

    }
  }

  /**
   * returns the class of the serializer, which would be used to serialize given object
   * @param obj to be serialized
   * @return null if given object is null, otherwise KryoSerializer.class.getName or JavaIOSerializer.class.getName
   */
  def evaluateSerializerClass(obj: Any): String =
  {

    if ( obj == null ) {
     // report("NULL -object can't be serialized", Severity.Error)
      return null
    } else {

      //TODO:re-enable when possible
      if ( obj.isInstanceOf[ UseJavaIOSerialization ] && !obj.asInstanceOf[ UseJavaIOSerialization ].isJavaIOSerializationDeprecated ) {
        return javaIOSerializerReference.getClass().getName
      } else if ( obj.isInstanceOf[ UseKryoSerialization ] && !obj.asInstanceOf[ UseKryoSerialization ].isKryoSerializationDeprecated ) {
        return kryoSerializerReference.getClass().getName
      } else {
                //report("object " + obj + " does not extend neither UseJavaIOSerialization nor UseKryoSerialization trait", Severity.Warning)
        return javaIOSerializerReference.getClass().getName // rest also with Java IO
      }

    }
  }

}
