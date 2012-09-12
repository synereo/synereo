package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import java.net.URI
import java.util.UUID

class AgentCnxnSerializer() extends Serializer[ AgentCnxn ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }


  override def write(kryo: Kryo, output: Output, obj: AgentCnxn): Unit =
  {
    if ( obj == null ) {
      kryo.writeObject(output, AgentCnxnSerializer.NULL)
    }
    else {
      System.err.println("KRYO AGENT_CNXN WRITE.... ")
      kryo.writeObject(output, AgentCnxnSerializer.NOT_NULL)
      kryo.writeObject(output, ( if ( obj.src != null ) {obj.src.toString} else {"NULL"} ))
      kryo.writeObject(output, ( if ( obj.trgt != null ) obj.trgt.toString else "NULL" ))

      var label = obj.label
      //      if ( label == "" )
      //        label = null
      kryo.writeObjectOrNull(output, label, classOf[ String ])
      kryo.writeObjectOrNull(output, obj.credential, classOf[ java.util.UUID ])
      System.err.println("KRYO MOCK_AGENT_CNXN SERIALIZE WRITE: " + obj)
    }
  }

  override def read(kryo: Kryo, input: Input, typ: Class[ AgentCnxn ]): AgentCnxn =
  {
    System.err.println("KRYO AGENT_CNXN READ.... ")
    val l: Byte = kryo.readObject(input, classOf[ Byte ])
    if ( l == AgentCnxnSerializer.NULL ) return null

    val src = kryo.readObject(input, classOf[ String ])
    val trgt = kryo.readObject(input, classOf[ String ])
    var label = kryo.readObjectOrNull(input, classOf[ String ])
    //    if ( label == null )
    //      label = ""
    val credential = kryo.readObjectOrNull(input, classOf[ java.util.UUID ])

    val cnxn = AgentCnxn(if ( !src.equals("NULL") ) URI.create(src) else null, label, if ( !trgt.equals("NULL") ) URI.create(trgt) else null, credential)
    System.err.println("KRYO AGENT_CNXN READ: " + cnxn)
    return cnxn

  }


}

object AgentCnxnSerializer
{
  val NULL: Byte = 0;
  val NOT_NULL: Byte = 1;

  def FITS_TO: Class[ AgentCnxn ] = classOf[ AgentCnxn ]
}

