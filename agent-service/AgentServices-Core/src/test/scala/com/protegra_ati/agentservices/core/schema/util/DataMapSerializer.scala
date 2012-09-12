package com.protegra_ati.agentservices.core.schema.util

import com.protegra_ati.agentservices.core.schema._
import com.esotericsoftware.kryo.serializers.MapSerializer
import com.esotericsoftware.kryo.io._
import com.esotericsoftware.kryo.Kryo

/* User: mgevantmakher
*/
//
//class DataMapSerializer[ Map[ AnyRef, T<:Data ] ](keyClass: AnyRef, valueClass: T) extends MapSerializer
//{
//  locally {
//    setImmutable(true)
//  }
//
//  override def write(kryo: Kryo, output: Output, obj: Map[ AnyRef, T ]): Unit =
//  {}
//
//  override def read(kryo: Kryo, input: Input, typ: Class[ Map[ AnyRef, T ] ]): Map[ AnyRef, T ] =
//  {
//    return null
//  }
//}