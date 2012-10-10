package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output


class NilSerializer extends Serializer[ List[ Nothing ] ]
{

  locally {
    setImmutable(true)
  }

  override def write(kryo: Kryo, output: Output, obj: List[ Nothing ]): Unit =
  {
   // System.err.println("KRYO NIL in USE! WRITE")
    kryo.writeObject(output, NilSerializer.NIL)
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ List[ Nothing ] ]): List[ Nothing ] =
  {
   // System.err.println("KRYO NIL in USE! READ")
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    return Nil
  }
}

object NilSerializer
{
  val NIL: Byte = 0;

  def FITS_TO: Class[ List[ Nothing ] ] = Nil.getClass.asInstanceOf[ Class[ List[ Nothing ] ] ]
}


