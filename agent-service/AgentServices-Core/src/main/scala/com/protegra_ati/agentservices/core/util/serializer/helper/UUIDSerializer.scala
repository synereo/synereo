package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import java.util.UUID

class UUIDSerializer() extends Serializer[ UUID ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }


  override def write(kryo: Kryo, output: Output, obj: UUID): Unit =
  {
    if ( obj == null ) {
      kryo.writeObject(output, UUIDSerializer.NULL)
    }
    else {
      kryo.writeObject(output, UUIDSerializer.NOT_NULL)
      System.err.println("KRYO UUID in USE! WRITE: " + obj.toString)
      kryo.writeObject(output, obj.toString.trim())
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ UUID ]): UUID =
  {

    val l: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( l == UUIDSerializer.NULL ) return null
    val o = kryo.readObject(input, classOf[ String ])
    System.err.println("KRYO UUID in USE! READ:" + o)
    return UUID.fromString(o.trim())
  }


}


object UUIDSerializer
{

  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ UUID ] = classOf[ UUID ]
}

