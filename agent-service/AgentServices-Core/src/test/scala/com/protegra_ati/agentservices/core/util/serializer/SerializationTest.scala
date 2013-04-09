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
  sequential

  def sd[T](in: T):T = {
    val ser = Serializer.serialize(in)
    ser mustNotEqual null

    val deser:T = Serializer.deserialize[T](ser)

    deser mustNotEqual null

    deser
  }

  "Kryo Serialization" should {
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

      out.source.channelRole mustEqual referral.source.channelRole
      out.source.channelLevel mustEqual referral.source.channelLevel

      out.channelRole mustEqual referral.channelRole
      out.channelLevel mustEqual referral.channelLevel

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

      out.source.channelRole mustEqual referral.source.channelRole
      out.source.channelLevel mustEqual referral.source.channelLevel

      out.channelRole mustEqual referral.channelRole
      out.channelLevel mustEqual referral.channelLevel

      out mustEqual referral
    }
  }
}