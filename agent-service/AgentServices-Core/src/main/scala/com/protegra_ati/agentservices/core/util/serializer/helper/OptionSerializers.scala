package com.protegra_ati.agentservices.core.util.serializer.helper

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

class NoneSerializer extends Serializer[ None.type ] {
  def write(kryo: Kryo, output: Output, obj: None.type) {}

  def read(kryo: Kryo, input: Input, typ: Class[None.type]) = None
}

object NoneSerializer {
  def FITS_TO: Class[None$] = classOf[ None$ ]
}

class SomeSerializer extends Serializer[ Option[_] ] {
  def write(kryo: Kryo, output: Output, obj: Option[_]) = kryo.writeClassAndObject(output, obj.get)

  def read(kryo: Kryo, input: Input, typ: Class[Option[_]]) = Some(kryo.readClassAndObject(input))
}

object SomeSerializer {
  def FITS_TO: Class[Some[_]] = classOf[ Some[_] ]
}