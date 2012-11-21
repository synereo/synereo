package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

class SomeOfStringSerializer() extends Serializer[ Some[ String ] ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }


  override def write(kryo: Kryo, output: Output, obj: Some[ String ]): Unit =
  {
    //println("KRYO SOME OF STRING in USE! WRITE:" + obj)
    if ( obj == null )
      kryo.writeObject(output, SomeOfStringSerializer.NULL)
    else {
      kryo.writeObject(output, SomeOfStringSerializer.NOT_NULL)
      kryo.writeObject(output, obj.get)
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ Some[ String ] ]): Some[ String ] =
  {
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == SomeOfStringSerializer.NULL ) return null

    val o = kryo.readObject(input, classOf[ String ])
   // println("KRYO SOME OF STRING in USE! READ:" + o)
    return Some(o)
  }

}

object SomeOfStringSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;
  val NONE: Byte = 3;
  val SOME: Byte = 4;

  def FITS_TO: Class[ Some[ String ] ] = classOf[ Some[ String ] ]

}


