package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

import com.protegra_ati.agentservices.core.messages.EventKey

class NoneSerializer extends Serializer[ AnyRef ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }

  override def write(kryo: Kryo, output: Output, obj: AnyRef): Unit =
  {
    System.err.println("KRYO NONE in USE! WRITE")
    if ( obj == null )
      kryo.writeObject(output, NoneSerializer.NULL)
    else kryo.writeObject(output, NoneSerializer.NOT_NULL)
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ AnyRef ]): AnyRef =
  {
    System.err.println("KRYO NONE in USE! READ")
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == NoneSerializer.NULL ) return null
    else return None
  }
}

object NoneSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ AnyRef ] = None.getClass.asInstanceOf[ Class[ AnyRef ] ]
}


