package com.protegra_ati.agentservices.store.dsl.protocols

import com.protegra_ati.agentservices.protocols._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import org.specs2.mutable.SpecificationWithJUnit

class ProtocolMessageTest extends SpecificationWithJUnit {
  "BeginIntroductionRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new BeginIntroductionRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(beginIntroductionRequest(_))".toLabel

      value must be_==(expected)
    }
  }

  "BeginIntroductionResponse" should {
    "convert to CnxnCtxtLabel" in {
      val responseId = UUID.randomUUID.toString
      val value = new BeginIntroductionResponse(responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(beginIntroductionResponse(responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "IntroductionRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new IntroductionRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(introductionRequest(_))".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionResponse" should {
    "convert to CnxnCtxtLabel" in {
      val responseId = UUID.randomUUID.toString
      val value = new IntroductionResponse(responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(introductionResponse(responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new GetIntroductionProfileRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(getIntroductionProfileRequest(_))".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileResponse" should {
    "convert to CnxnCtxtLabel" in {
      val responseId = UUID.randomUUID.toString
      val value = new GetIntroductionProfileResponse(responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(getIntroductionProfileResponse(responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "Connect" should {
    "convert to CnxnCtxtLabel" in {
      val connectId = UUID.randomUUID.toString
      val value = new Connect(connectId).toCnxnCtxtLabel
      val expected = ("protocolMessage(connect(connectId(\"" + connectId + "\")))").toLabel

      value must be_==(expected)
    }
  }
}
