package com.protegra_ati.agentservices.store.dsl.protocols

import com.protegra_ati.agentservices.protocols.msgs._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import org.specs2.mutable.SpecificationWithJUnit

class ProtocolMessageTest extends SpecificationWithJUnit {
  "BeginIntroductionRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new BeginIntroductionRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(beginIntroductionRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "BeginIntroductionResponse" should {
    "convert to CnxnCtxtLabel" in {
      val sessionId = UUID.randomUUID.toString
      val responseId = UUID.randomUUID.toString
      val value = new BeginIntroductionResponse(Some(sessionId), responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(beginIntroductionResponse(sessionId(\"" + sessionId + "\"),responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "IntroductionRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new IntroductionRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(introductionRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "IntroductionResponse" should {
    "convert to CnxnCtxtLabel" in {
      val sessionId = UUID.randomUUID.toString
      val responseId = UUID.randomUUID.toString
      val value = new IntroductionResponse(Some(sessionId), responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(introductionResponse(sessionId(\"" + sessionId + "\"),responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileRequest" should {
    "convert to CnxnCtxtLabel" in {
      val value = new GetIntroductionProfileRequest().toCnxnCtxtLabel
      val expected = "protocolMessage(getIntroductionProfileRequest(sessionId(_)))".toLabel

      value must be_==(expected)
    }
  }

  "GetIntroductionProfileResponse" should {
    "convert to CnxnCtxtLabel" in {
      val sessionId = UUID.randomUUID.toString
      val responseId = UUID.randomUUID.toString
      val value = new GetIntroductionProfileResponse(Some(sessionId), responseId).toCnxnCtxtLabel
      val expected = ("protocolMessage(getIntroductionProfileResponse(sessionId(\"" + sessionId + "\"),responseId(\"" + responseId + "\")))").toLabel

      value must be_==(expected)
    }
  }

  "Connect" should {
    "convert to CnxnCtxtLabel" in {
      val sessionId = UUID.randomUUID.toString
      val connectId = UUID.randomUUID.toString
      val value = new Connect(Some(sessionId), connectId).toCnxnCtxtLabel
      val expected = ("protocolMessage(connect(sessionId(\"" + sessionId + "\"),connectId(\"" + connectId + "\")))").toLabel

      value must be_==(expected)
    }
  }
}
