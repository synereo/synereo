package com.protegra_ati.agentservices.core.schema.util

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import com.protegra_ati.agentservices.core.messages.{Identification, MockMessage}
import com.protegra_ati.agentservices.core.schema.MockEventKey


class MockMessageSerializer() extends Serializer[ MockMessage ]
{

  locally {
    setImmutable(true)
  }


  override def write(kryo: Kryo, output: Output, obj: MockMessage): Unit =
  {

    System.err.println("KRYO MOCK_MESSAGE IN USE! WRITE: " + obj.toString)
    kryo.writeObject(output, obj.ids)
    kryo.writeObject(output, obj.eventKey)
    kryo.writeObject(output, obj.eventKey2)

  }

  override def read(kryo: Kryo, input: Input, typ: Class[ MockMessage ]): MockMessage =
  {


    val id = kryo.readObject(input, classOf[ Identification ])
    val eKey = kryo.readObject(input, classOf[ MockEventKey ])
    val eKey2 = kryo.readObject(input, classOf[ MockEventKey ])
    System.err.println("KRYO MOCK_MESSAGE  in USE! READ")
    return new MockMessage(id,eKey,  eKey2)

  }


}

object MockMessageSerializer
{
  def FITS_TO: Class[ MockMessage ] = classOf[ MockMessage ]
}

