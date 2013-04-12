package com.ati.iaservices.helpers

import org.specs2.mutable._
import java.util.UUID
import com.protegra_ati.agentservices.core.util.Results
import org.specs2.time.Duration
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

class HelperTest extends SpecificationWithJUnit
with Serializable
{
  val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
  val store = new CreateStoreHelper().createStore
  val ui = new CreateUIHelper().createUI

  "RegisterAgentHelper" should {
    "Create a new agent" in {
      val resultKey = Results.getKey()
      val agentSessionId = UUID.randomUUID

      val registerAgentHelper = new RegisterAgentHelper() {
        def handleListen(response: RegistrationResponse) {
          if (response.agentId != null) {
            println("*************** NewAgentId = " + response.agentId + " ***************" )
            println
            Results.trigger(resultKey)
          }
        }
      }
      val eventKey = "Register" + UUID.randomUUID().toString
      registerAgentHelper.listen(ui, agentSessionId, eventKey)
      registerAgentHelper.request(ui, agentSessionId, eventKey, BIZNETWORK_AGENT_ID, "John Smith")

      Results.triggered(resultKey) must be_==(true).eventually(5, new Duration(2000))
    }
  }

  "SetContentHelper with Profile" should {
    "Create a profile" in {
      val resultKey = Results.getKey()
      val agentSessionId = UUID.randomUUID

      val registerAgentHelper = new RegisterAgentHelper() {
        def handleListen(response: RegistrationResponse) {
          if (response.agentId != null) {
            println("*************** NewAgentId = " + response.agentId + " ***************" )
            println
            val setContentHelper = new SetContentHelper[Profile] {
              def handleListen(profile: Profile) = {
                if (profile != null && profile.firstName.equals("John")) {
                  Results.trigger(resultKey)
                }
              }
            }
            val eventKey = "Set_Profile"
            val target = new AgentCnxnProxy(response.agentId.toString.toURI, "", response.agentId.toString.toURI)
            val profile = new Profile("John","Smith", "", "", "Canada", "", "", "", "")
            setContentHelper.listen(ui, agentSessionId, eventKey)
            setContentHelper.request(ui, agentSessionId, eventKey, profile, target)
          }
        }
      }
      val eventKey = "Register" + UUID.randomUUID().toString
      registerAgentHelper.listen(ui, agentSessionId, eventKey)
      registerAgentHelper.request(ui, agentSessionId, eventKey, BIZNETWORK_AGENT_ID, "John Smith")

      Results.triggered(resultKey) must be_==(true).eventually(5, new Duration(2000))
    }
  }

}
