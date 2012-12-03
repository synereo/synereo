package com.protegra_ati.agentservices.core.schema.util

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import scala.Option
import com.protegra_ati.agentservices.core.messages.MockMessage


//class MockMessageSerializer() extends Serializer
//{
//
//  locally {
//    setImmutable(true)
//  }
//
//  override def write(kryo: Kryo, output: Output, obj: MockMessage): Unit =
//  {
//    kryo.writeObject(output, obj)
//
//  }
//
//  override def read(kryo: Kryo, input: Input, typ: Class[ MockMessage ]): MockMessage =
//  {
//    null.asInstanceOf[MockMessage]
////    println("KRYO Option in USE! READ")
////    val optionType = input.readBoolean()
////    if ( !optionType )
////      return None
////    val someClass = kryo.readClass(input).getType
////    return Some(kryo.readObject(input, someClass).asInstanceOf[ AnyRef ])
//  }
//
////  private def cacheSomeValue(obj: Option[ AnyRef ]): Class[ _ ] =
////  {
////    val claz = obj.get.getClass
////    return claz
////  }
//
//
//}
//
//object MessageSerializer
//{
//  def FITS_TO: Class[ MockMessage ] = classOf[ MockMessage ]
//}
