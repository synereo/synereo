package com.protegra_ati.agentservices.store.dsl.protocols

import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import org.specs2.mutable.SpecificationWithJUnit

class ProtocolMessageTest extends SpecificationWithJUnit {
  "BeginIntroductionRequest" should {
    "convert to Listen Label" in {
      val value = BeginIntroductionRequest.toLabel()
      val expected = "protocolMessage(beginIntroductionRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "BeginIntroductionRequest" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val value = BeginIntroductionRequest(sessionId, null, null, null, null).toLabel
      val expected = s"""protocolMessage(beginIntroductionRequest(sessionId(\"$sessionId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "Connect" should {
    "convert to Listen Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = Connect.toLabel(sessionId, correlationId)
      val expected = s"""protocolMessage(connect(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "Connect" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = Connect(sessionId, correlationId, false, null).toLabel
      val expected = s"""protocolMessage(connect(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "ConnectNotification" should {
    "convert to Listen Label" in {
      val value = ConnectNotification.toLabel()
      val expected = "protocolMessage(connectNotification(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "ConnectNotification" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val value = ConnectNotification(sessionId, null, null).toLabel
      val expected = s"""protocolMessage(connectNotification(sessionId(\"$sessionId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileRequest" should {
    "convert to Listen Label" in {
      val value = GetIntroductionProfileRequest.toLabel()
      val expected = "protocolMessage(getIntroductionProfileRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileRequest" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val value = GetIntroductionProfileRequest(sessionId, null, null).toLabel
      val expected = s"""protocolMessage(getIntroductionProfileRequest(sessionId(\"$sessionId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileResponse" should {
    "convert to Listen Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = GetIntroductionProfileResponse.toLabel(sessionId, correlationId)
      val expected = s"""protocolMessage(getIntroductionProfileResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileResponse" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = GetIntroductionProfileResponse(sessionId, correlationId, null).toLabel
      val expected = s"""protocolMessage(getIntroductionProfileResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionConfirmation" should {
    "convert to Listen Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = IntroductionConfirmation.toLabel(sessionId, correlationId)
      val expected = s"""protocolMessage(introductionConfirmation(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionConfirmation" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = IntroductionConfirmation(sessionId, correlationId, false).toLabel
      val expected = s"""protocolMessage(introductionConfirmation(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionNotification" should {
    "convert to Listen Label" in {
      val value = IntroductionNotification.toLabel()
      val expected = "protocolMessage(introductionNotification(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionNotification" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val value = IntroductionNotification(sessionId, null, null, null, null).toLabel
      val expected = s"""protocolMessage(introductionNotification(sessionId(\"$sessionId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionRequest" should {
    "convert to Listen Label" in {
      val value = IntroductionRequest.toLabel()
      val expected = "protocolMessage(introductionRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionRequest" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val value = IntroductionRequest(sessionId, null, null, null, null).toLabel
      val expected = s"""protocolMessage(introductionRequest(sessionId(\"$sessionId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionResponse" should {
    "convert to Listen Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = IntroductionResponse.toLabel(sessionId, correlationId)
      val expected = s"""protocolMessage(introductionResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionResponse" should {
    "convert to Send Label" in {
      val sessionId = UUID.randomUUID.toString
      val correlationId = UUID.randomUUID.toString
      val value = IntroductionResponse(sessionId, correlationId, false, null).toLabel
      val expected = s"""protocolMessage(introductionResponse(sessionId(\"$sessionId\"),correlationId(\"$correlationId\")))""".toLabel

      value must be_==(expected)
    }
  }
}
