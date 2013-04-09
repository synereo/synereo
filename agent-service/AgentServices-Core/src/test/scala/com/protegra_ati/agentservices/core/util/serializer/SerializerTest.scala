package com.protegra_ati.agentservices.core.util.serializer

import org.specs2.mutable.SpecificationWithJUnit
import scala.concurrent.ThreadPoolRunnersX
import com.protegra_ati.agentservices.core.messages.ChannelLevel

class SerializerTest
  extends SpecificationWithJUnit
  with ThreadPoolRunnersX
{
  sequential

  "Kryo Serialization" should {
    "Serialize Some(String) properly" in {
      val o:Option[String] = Some("Foobar")

      val ser = Serializer.serialize(o)
      ser should_!=(null)

      val deser:Option[String] = Serializer.deserialize[Option[String]](ser)

      deser mustNotEqual null
      deser.get mustEqual "Foobar"
    }

    "Serialize Some(Enum) propertly" in {
      val o:Option[ChannelLevel.Value] = Some(ChannelLevel.Private)

      val ser = Serializer.serialize(o)
      ser mustNotEqual null

      val deser:Option[ChannelLevel.Value] = Serializer.deserialize[Option[ChannelLevel.Value]](ser)

      deser mustNotEqual null
      //deser.get.should_==(inVal)
      deser.get mustEqual ChannelLevel.Private
    }

    "Serialize Some(null) propertly" in {
      val o:Option[ChannelLevel.Value] = Some(null)

      val ser = Serializer.serialize(o)
      ser mustNotEqual null

      val deser:Option[ChannelLevel.Value] = Serializer.deserialize[Option[ChannelLevel.Value]](ser)

      deser mustNotEqual null
      deser.get mustEqual null
    }
  }
}