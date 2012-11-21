package com.protegra_ati.agentservices.core.schema.util

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.messages.{Identification, MockMessage}
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema._
import java.net.URI
import java.util.UUID

abstract class MockAgentCnxnProxySerializer() extends Serializer[ MockAgentCnxnProxy ]
{

  locally {
    setImmutable(true)
    setAcceptsNull(true)
  }


//  override def write(kryo: Kryo, output: Output, obj: MockAgentCnxnProxy): Unit =
//  {
//    println("KRYO MOCK_AGENT_CNXN WRITE.... ")
//    kryo.writeObject(output, obj.src)
//    kryo.writeObject(output, obj.trgt)
//
//    var label = obj.label
//    if (label == "")
//      label = null
//    kryo.writeObjectOrNull(output, label, classOf[String])
//    kryo.writeObject(output, obj.credential.toString)
//    println("KRYO MOCK_AGENT_CNXN SERIALIZE WRITE: " + obj)
//
//  }

//  override def read(kryo: Kryo, input: Input, typ: Class[ MockAgentCnxnProxy ]): MockAgentCnxnProxy =
//  {
//    println("KRYO MOCK_AGENT_CNXN READ.... ")
//    val src = kryo.readObject(input, classOf[ URI ])
//    val trgt = kryo.readObject(input, classOf[ URI ])
//    var label = kryo.readObjectOrNull(input, classOf[ String ])
//    if (label == null)
//      label = ""
//    val credential = UUID.fromString(kryo.readObject(input, classOf[ String ]))
//
//
//    val cnxn = MockAgentCnxnProxy(src, label, trgt, credential)
//    println("KRYO MOCK_AGENT_CNXN READ: " + cnxn)
//    return cnxn
//
//  }


  def writePoliciesList(kryo: Kryo, output: Output, list: java.util.List[ ConnectionPolicy.Value ]): Unit =
  {
    if ( list == null ) kryo.writeObject(output, "null")
    else if ( list.isEmpty ) kryo.writeObject(output, "empty")
    else {
      kryo.writeObject(output, "" + list.size())
      val iterator: Iterator[ ConnectionPolicy.Value ] = list.iterator()
      while ( iterator.hasNext ) {
        kryo.writeObject(output, iterator.next().toString)
      }
      kryo.writeObject(output, "end")
    }
  }

  def readPoliciesList(kryo: Kryo, input: Input): java.util.List[ ConnectionPolicy.Value ] =
  {
    val size = kryo.readObject(input, classOf[ String ])
    if ( size.equals("null") ) return null
    else if ( size.equals("empty") ) return List[ ConnectionPolicy.Value ]()
    else {
      val collectionSize: Int = size.toInt
      var outputList = List[ ConnectionPolicy.Value ]()
      for ( i <- 0 until collectionSize ) {
        outputList = ( ConnectionPolicy.withName(kryo.readObject(input, classOf[ String ])) ) :: outputList
      }
      val end = kryo.readObject(input, classOf[ String ])
      if ( end.equals("end") )
        return outputList
      else null
    }

  }

}

object MockAgentCnxnProxySerializer
{
  def FITS_TO: Class[ MockAgentCnxnProxy ] = classOf[ MockAgentCnxnProxy ]
}

