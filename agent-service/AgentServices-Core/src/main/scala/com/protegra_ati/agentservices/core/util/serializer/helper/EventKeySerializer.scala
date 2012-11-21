package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import com.protegra_ati.agentservices.core.messages.EventKey
import java.util.UUID


class EventKeySerializer() extends Serializer[ EventKey ]
{

  locally {
    //setImmutable(true)
    setImmutable(true);
    setAcceptsNull(true);
  }

  override def write(kryo: Kryo, output: Output, obj: EventKey): Unit =
  {
    //  ## error here somewhere
    if ( obj == null )
      kryo.writeObject(output, EventKeySerializer.NULL)
    else {
      kryo.writeObject(output, EventKeySerializer.NOT_NULL)
     // println("KRYO EventKeySerializer in USE! TO WRITE" + obj)
      kryo.writeObjectOrNull(output, obj.agentSessionId, classOf[ java.util.UUID ])
      kryo.writeObjectOrNull(output, obj.eventTag, classOf[ String ])
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ EventKey ]): EventKey =
  {
    //println("KRYO EventKeySerializer in USE! READ")
    val label: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( label == EventKeySerializer.NULL ) {
    //  println("EventKey IS NULL")
      return null
    }
    //println("EventKey NOT NULL")
    val agentSessionId = kryo.readObjectOrNull(input, classOf[ java.util.UUID ])
    // TODO eventuell nullcheck for UUID
    val eventTag = kryo.readObjectOrNull(input, classOf[ String ])
    //println("KRYO EventKeySerializer deserialized =" + eventTag + ", sesID:" + agentSessionId)
    return new EventKey(agentSessionId, eventTag)
  }

  //  override def copy(kryo: Kryo, original: EventKey): EventKey =
  //  {
  //    val copy: EventKey = new EventKey(original.agentSessionId, original.eventTag)
  //    return copy;
  //  }
}

object EventKeySerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ EventKey ] = classOf[ EventKey ]
}
