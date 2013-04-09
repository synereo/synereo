package com.protegra_ati.agentservices.core.util.serializer.helper

import collection.mutable.ArrayBuffer
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Output, Input}
import scala.collection.immutable

class EnumerationSerializer extends Serializer[Enumeration#Value] {
  val ref = new Object

  // Cache the mapping from enumeration members to specific enumeration classes
  var value2enumClass = immutable.Map[Enumeration#Value, Class[_]]()
  // Cache enumeration values for a given enumeration class
  var enumClass2enumValues = immutable.Map[Class[_], ArrayBuffer[Enumeration#Value]]()

  private def cacheEnumValue(obj: Enumeration#Value) = {
    val enumClass =  value2enumClass.get(obj) getOrElse {
      val parentEnum = obj.asInstanceOf[AnyRef].getClass.getSuperclass.getDeclaredFields.find( f => f.getName == "$outer" ).get
      val parentEnumObj = parentEnum.get(obj)
      val enumClass = parentEnumObj.getClass
      value2enumClass += obj->enumClass
      val enumValues =  enumClass2enumValues.get(enumClass) getOrElse {
        val size = parentEnumObj.asInstanceOf[Enumeration].maxId+1
        val values = new ArrayBuffer[Enumeration#Value](size)
        0 until size foreach { e => values += null }
        enumClass2enumValues += enumClass->values
        values
      }
      enumClass
    }

    val enumValues =  enumClass2enumValues.get(enumClass).get

    if(enumValues(obj.id) == null) {
      enumValues.update(obj.id, obj)
    }

    enumClass
  }

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

    val enumValues =  enumClass2enumValues.get(clazz).getOrElse {
      cacheEnumValue(kryo.newInstance(clazz).asInstanceOf[Enumeration](id))
      enumClass2enumValues.get(clazz).get
    }

    val enumInstance = enumValues(id)
    enumInstance
  }
}

object EnumerationSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ scala.Enumeration#Value ] = classOf[ scala.Enumeration#Value ]

  def FITS_TO1: Class[ scala.Enumeration$Val ] = classOf[ scala.Enumeration$Val ]
}
