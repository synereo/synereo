package com.protegra_ati.agentservices.core.schema.util

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import com.protegra_ati.agentservices.core.messages.EventKey
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.MockEventKey


class MockEventKeySerializer() extends Serializer[ MockEventKey ]
{

  locally {
    //setImmutable(true)
   // setImmutable(true);
   setAcceptsNull(true);
  }

  override def write(kryo: Kryo, output: Output, obj: MockEventKey): Unit =
  {
    //  ## error here somewhere
    //System.err.println("KRYO  MockEventKeySerializer in USE! WRITE")
    kryo.writeObject(output, obj.agentSessionId)
    kryo.writeObject(output, obj.eventTag)
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ MockEventKey ]): MockEventKey =
  {
    //System.err.println("KRYO  MockEventKeySerializer in USE! READ")
    val agentSessionId = kryo.readObject(input, classOf[ UUID ])
    val eventTag = kryo.readObject(input, classOf[ String ])
   // System.err.println("MockEventKeySerializer read:=" + eventTag + ", sesID:" + agentSessionId)
    return new MockEventKey(agentSessionId, eventTag)
  }


  override def copy(kryo: Kryo, original: MockEventKey): MockEventKey =
  {
    val copy: MockEventKey = new MockEventKey(original.agentSessionId, original.eventTag)
    return copy;
  }
}

object MockEventKeySerializer
{
  def FITS_TO: Class[ MockEventKey ] = classOf[ MockEventKey ]
}
