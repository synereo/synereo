package com.protegra_ati.agentservices.core.platformagents

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import org.junit._
import Assert._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import invitation._

import org.specs2.mutable._
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core._
import java.util.{Locale, UUID}
import com.protegra_ati.agentservices.core.util.Results
import com.protegra_ati.agentservices.core.messages.admin.{RegistrationResponse, RegistrationRequest}

class AgentHostCombinedRegistrationTest extends SpecificationWithJUnit
with Timeouts
with InvitationUser
with ReferralUser
{
  val cnxnUIStore = new AgentCnxnProxy(( "UI" + UUID.randomUUID().toString ).toURI, "", ( "Store" + UUID.randomUUID().toString ).toURI)
  val storeR = new AgentHostStorePlatformAgent
  val uiR = new AgentHostUIPlatformAgent
  AgentHostCombinedBase.setupPAs(storeR, uiR, cnxnUIStore)

  "Registrations" should {
    "send RegistrationResponse from RegistrationRequest" in {
      val selfAlias = "John Smith"
      val agentSessionId = UUID.randomUUID
      val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
      val eventKey = "registration"
      val key = Results.getKey()

      val userAgentId = UUID.randomUUID
      val selfConnection = AgentHostCombinedBase.setupConnection(storeR, userAgentId, userAgentId)
      storeR.addToHostedCnxn(selfConnection.writeCnxn)

      def requestRegistration(ui: AgentHostUIPlatformAgent, selfAlias: String, agentSessionId: UUID, tag: String) =
      {
        val req = new RegistrationRequest(new EventKey(agentSessionId, tag), BIZNETWORK_AGENT_ID, selfAlias)
        req.targetCnxn = selfConnection.readCnxn
        ui.send(req)
      }

      def assertRegistrationResponse(ui: AgentHostUIPlatformAgent, agentSessionId: UUID, tag: String) =
      {
        ui.addListener(agentSessionId, "", new MessageEventAdapter(tag)
        {
          override def registrationResponseReceived(e: RegistrationResponseReceivedEvent) =
          {

            println("***********registration response received ---------------------------:" + e.toString)
            val newAgentId = e.msg.asInstanceOf[RegistrationResponse].agentId
            Results.trigger(key)
          }

        });

        Results.triggered(key) must be_==(true).eventually(15, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
      }

      requestRegistration(uiR, selfAlias, agentSessionId, eventKey)
      Thread.sleep(TIMEOUT_SHORT)
      assertRegistrationResponse(uiR, agentSessionId, eventKey);
    }
  }
}
