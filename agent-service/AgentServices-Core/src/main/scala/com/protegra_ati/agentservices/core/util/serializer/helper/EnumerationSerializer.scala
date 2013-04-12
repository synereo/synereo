package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.Enumeration
import com.protegra_ati.agentservices.store.util.{Severity, Reporting}

class EnumerationSerializer
  extends Serializer[Enumeration#Value]
  with Reporting
{
  import EnumerationSerializer._

  override def write (kryo: Kryo, output: Output, obj: Enumeration#Value) = {
    val enumClass = cacheEnumValue(obj)
    // Output a specific class of the enumeration
    kryo.writeClass(output, enumClass)
    output.writeInt(obj.id)
  }

  override def read (kryo: Kryo, input: Input, typ: Class[Enumeration#Value]): Enumeration#Value = {
    // Read a specific class of the enumeration
    val clazz = kryo.readClass(input).getType
    val id = input.readInt()

    val enumValues =  this.synchronized {
      enumClass2enumValues.get(clazz).getOrElse {
        cacheEnumValue(kryo.newInstance(clazz).asInstanceOf[Enumeration](id))
        enumClass2enumValues.get(clazz).get
      }
    }

    val enumInstance = enumValues(id)
    if (enumInstance == null) {
      report("Error while deserializing Enum.  Enum value was null: %s[%s]".format(clazz, id), Severity.Error)
    }
    enumInstance
  }
}

object EnumerationSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ scala.Enumeration#Value ] = classOf[ scala.Enumeration#Value ]
  def ENUM_SER_CLASS_OF: Class[ EnumerationSerializer ] = classOf[ EnumerationSerializer ]

  // Cache the mapping from enumeration members to specific enumeration classes
  var value2enumClass = immutable.Map[Enumeration#Value, Class[_]]()
  // Cache enumeration values for a given enumeration class
  var enumClass2enumValues = immutable.Map[Class[_], ArrayBuffer[Enumeration#Value]]()

  private def cacheEnumValue(obj: Enumeration#Value) = {
    this.synchronized {
      val outEnumClass =  value2enumClass.get(obj) getOrElse {
        val parentEnum = obj.asInstanceOf[AnyRef].getClass.getSuperclass.getDeclaredFields.find( f => f.getName == "$outer" ).get
        val parentEnumObj = parentEnum.get(obj)
        val inEnumClass = parentEnumObj.getClass
        value2enumClass += obj->inEnumClass
        val enumValues =  enumClass2enumValues.get(inEnumClass) getOrElse {
          val enumInstance = parentEnumObj.asInstanceOf[Enumeration]
          val size = enumInstance.maxId
          val values = new ArrayBuffer[Enumeration#Value](size)
          //0 until size foreach { e => values += null }
          0 until size foreach { e => values += enumInstance(e)}
          enumClass2enumValues += inEnumClass->values
          values
        }
        inEnumClass
      }

      val enumValues =  enumClass2enumValues.get(outEnumClass).get

      if(enumValues(obj.id) == null) {
        enumValues.update(obj.id, obj)
      }

      outEnumClass
    }
  }
}
