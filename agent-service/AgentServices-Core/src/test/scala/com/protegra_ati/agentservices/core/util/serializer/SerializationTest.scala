package com.protegra_ati.agentservices.core.util.serializer

import org.specs2.mutable.SpecificationWithJUnit
import scala.concurrent.ThreadPoolRunnersX
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import com.protegra_ati.agentservices.core.messages.invitation.{CreateInvitationRequest, ReferralRequest}
import java.util.UUID
import com.protegra_ati.agentservices.core.schema.Post
import com.protegra_ati.agentservices.core.messages.EventKey
import scala.Some
import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}

class SerializationTest
  extends SpecificationWithJUnit
  with ThreadPoolRunnersX
{
  def sd[T](in: T):T = {
    val ser = Serializer.serialize(in)
    ser mustNotEqual null

    val deser:T = Serializer.deserialize[T](ser)

    deser mustNotEqual null

    deser
  }

  "Kryo Serialization" should {
    "Serialize None" in {
      val o:Option[String] = None
      val out = sd[Option[String]](o)
      out mustEqual None
    }

    "Serialize Some(String)" in {
      Some(ChannelLevel.Private) mustEqual Option(ChannelLevel.Private)

      val o:Option[String] = Some("Foobar")

      val out = sd[Option[String]](o)
      out.get mustEqual "Foobar"
    }

    "Serialize Some(Enum)" in {
      val o:Option[ChannelLevel.Value] = Option(ChannelLevel.Private)

      val out = sd[Option[ChannelLevel.Value]](o)
      out.get mustEqual ChannelLevel.Private
    }

    "Serialize Some(null)" in {
      val o:Option[ChannelLevel.Value] = Some(null)

      val out = sd[Option[ChannelLevel.Value]](o)
      out.get mustEqual null
    }

    "Serialize Empty ReferralRequest"  in {
      val rr1 = new ReferralRequest()

      val out1 = sd[ReferralRequest](rr1)
      out1 mustEqual rr1
    }

    "Serialize Populated ReferralRequest" in {
      val postToTarget = new Post()
      val postToBroker = new Post()

      val invitation = new CreateInvitationRequest(
        new Identification,
        new EventKey(UUID.randomUUID(), "Invitation"),
        UUID.randomUUID.toString,
        "SelfAlias",
        "TargetAlias",
        "SelfCategory",
        "TargetCategory",
        "RequestedConnectionType",
        "RequestedConnectionName",
        postToTarget,
        postToBroker,
        true
      )

      invitation.channelRole mustEqual Option(ChannelRole.Creator)
      invitation.channelLevel mustEqual None

      val referral = new ReferralRequest(new Identification, new EventKey(UUID.randomUUID, "Request"), invitation)

      referral.channelRole mustEqual Option(ChannelRole.Creator)
      referral.channelLevel mustEqual Option(ChannelLevel.Public)

      val out = sd[ReferralRequest](referral)

      val h1 = Integer.toHexString(System.identityHashCode(out.source.channelRole.getOrElse(None)))
      val h2 = Integer.toHexString(System.identityHashCode(invitation.channelRole.getOrElse(None)))
      val h3 = Integer.toHexString(System.identityHashCode(ChannelRole.Creator))

      println("H1: %s (Value: %s)".format(h1, out.source.channelRole.getOrElse(None)))
      println("H2: %s (Value: %s)".format(h2, invitation.channelRole.getOrElse(None)))
      println("H3: %s (Value: %s)".format(h3, ChannelRole.Creator))
      println("")

      out.source.channelRole.getOrElse(None) mustEqual referral.source.channelRole.getOrElse(None)
      out.source.channelLevel.getOrElse(None) mustEqual referral.source.channelLevel.getOrElse(None)

      out.channelRole.getOrElse(None) mustEqual referral.channelRole.getOrElse(None)

      val hashEnum1 = Integer.toHexString(System.identityHashCode(out.channelLevel.getOrElse(None)))
      val hashEnum2 = Integer.toHexString(System.identityHashCode(referral.channelLevel.getOrElse(None)))
      val hashEnum3 = Integer.toHexString(System.identityHashCode(ChannelLevel.Public))

      if (!out.channelLevel.getOrElse(None).equals(referral.channelLevel.getOrElse(None)))
        println("NO MATCH!")

      println("Hash Enum 1: " + hashEnum1)
      println("Hash Enum 2: " + hashEnum2)
      println("Hash Enum 3: " + hashEnum3)
      println("")

      out.channelLevel.getOrElse(None) mustEqual referral.channelLevel.getOrElse(None)

      out mustEqual referral
    }

    "Serialize Populated ReferralRequest with Some(null)" in {
      val postToTarget = new Post()
      val postToBroker = new Post()

      val invitation = new CreateInvitationRequest(
        new Identification,
        new EventKey(UUID.randomUUID(), "Invitation"),
        UUID.randomUUID.toString,
        "SelfAlias",
        "TargetAlias",
        "SelfCategory",
        "TargetCategory",
        "RequestedConnectionType",
        "RequestedConnectionName",
        postToTarget,
        postToBroker,
        true
      )

      invitation.channelRole = Some(ChannelRole.Store)
      invitation.channelLevel = None

      val referral = new ReferralRequest(new Identification, new EventKey(UUID.randomUUID, "Request"), invitation)

      referral.channelRole = None
      referral.channelLevel = Some(null)

      val out = sd[ReferralRequest](referral)

      out.source.channelRole.getOrElse(None) mustEqual referral.source.channelRole.getOrElse(None)
      out.source.channelLevel.getOrElse(None) mustEqual referral.source.channelLevel.getOrElse(None)

      out.channelRole.getOrElse(None) mustEqual referral.channelRole.getOrElse(None)
      out.channelLevel.getOrElse(None) mustEqual referral.channelLevel.getOrElse(None)

      out mustEqual referral
    }
  }
}

