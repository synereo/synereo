package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output

import java.util.UUID
import com.protegra_ati.agentservices.core.messages.Identification

//https://github.com/ajaegle/kryo-serializers/blob/kryo2/src/main/java/de/javakaffee/kryoserializers/jodatime/JodaDateTimeSerializer.java
class IdentificationSerializer() extends Serializer[ Identification ]
{

  locally {
    // setImmutable(true)
    setAcceptsNull(true)
  }

  override def write(kryo: Kryo, output: Output, obj: Identification): Unit =
  {
    System.err.println("KRYO IdentificationSerializer in USE! WRITE")
    if ( obj == null ) {
      kryo.writeObject(output, IdentificationSerializer.NULL)
    }
    else {
      kryo.writeObject(output, IdentificationSerializer.NOT_NULL)
      kryo.writeObjectOrNull(output, obj.conversationId, classOf[ String ])
      kryo.writeObjectOrNull(output, obj.parentId, classOf[ String ])
      kryo.writeObjectOrNull(output, obj.id, classOf[ String ])
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ Identification ]): Identification =
  {
    System.err.println("KRYO IdentificationSerializer in USE! READ")
    val l: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( l == IdentificationSerializer.NULL ) return null

    val conversationId = kryo.readObjectOrNull(input, classOf[ String ])
    val parentId = kryo.readObjectOrNull(input, classOf[ String ])
    val id = kryo.readObjectOrNull(input, classOf[ String ])
    return new Identification(id, parentId, conversationId)
  }
}

object IdentificationSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ Identification ] = classOf[ Identification ]
}
