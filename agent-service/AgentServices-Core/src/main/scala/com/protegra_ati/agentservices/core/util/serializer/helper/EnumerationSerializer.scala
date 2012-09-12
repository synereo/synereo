package com.protegra_ati.agentservices.core.util.serializer.helper

import collection.mutable.ArrayBuffer
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Output, Input}
import scala.collection.immutable


/* User: mgevantmakher
*/

class EnumerationSerializer extends Serializer[ Enumeration#Value ]
{
  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }

  // Caching of parent enum types for value types
  var valueClass2enumClass = immutable.Map[ Class[ _ ], Class[ _ ] ]()
  // Cache enumeration values for a given enumeration class
  var enumClass2enumValues = immutable.Map[ Class[ _ ], ArrayBuffer[ Enumeration#Value ] ]()

  private def cacheEnumValue(obj: Enumeration#Value) =
  {

    val enumClass = valueClass2enumClass.get(obj.getClass) getOrElse {
      val parentEnum = obj.asInstanceOf[ AnyRef ].getClass.getSuperclass.getDeclaredFields.find(f => f.getName == "$outer").get
      val parentEnumObj = parentEnum.get(obj)
      val enumClass = parentEnumObj.getClass
      valueClass2enumClass += obj.getClass -> enumClass
      val enumValues = enumClass2enumValues.get(enumClass) getOrElse {
        val size = parentEnumObj.asInstanceOf[ Enumeration ].maxId + 1
        val values = new ArrayBuffer[ Enumeration#Value ](size)
        0 until size foreach {e => values += null}
        enumClass2enumValues += enumClass -> values
        values
      }
      enumClass
    }

    val enumValues = enumClass2enumValues.get(enumClass).get

    if ( enumValues(obj.id) == null ) {
      enumValues.update(obj.id, obj)
    }

    enumClass
  }

  override def write(kryo: Kryo, output: Output, obj: Enumeration#Value) =
  {
    System.err.println("KRYO EnumSerializer in USE! WRITE")
    if ( obj == null )
      kryo.writeObject(output, EnumerationSerializer.NULL)
    else {
      kryo.writeObject(output, EnumerationSerializer.NOT_NULL)
      val enumClass = cacheEnumValue(obj)
      kryo.writeClass(output, enumClass)
      output.writeInt(obj.id)
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ Enumeration#Value ]): Enumeration#Value =
  {
    System.err.println("KRYO EnumSerializer in USE! READ")
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == EnumerationSerializer.NULL ) return null

    val clazz = kryo.readClass(input).getType
    val id = input.readInt()

    val enumValues = enumClass2enumValues.get(clazz).getOrElse {
      cacheEnumValue(kryo.newInstance(clazz).asInstanceOf[ Enumeration ](id))
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
