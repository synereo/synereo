package com.ati.iaservices.helpers

import org.specs2.mutable._
import java.util.UUID
import com.protegra_ati.agentservices.core.util.Results
import org.specs2.time.Duration
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.schema.{CompositeData, Connection, AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util
import com.ati.iaservices.schema.{LabelKey, MessageContent, Label}

class HelperTest extends SpecificationWithJUnit
with Serializable {
  val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
  val store = new CreateStoreHelper().createStore()
  val ui = new CreateUIHelper().createUI()

  "RegisterAgentHelper" should {
    "Create a new agent" in {
      val resultKey = Results.getKey()
      val agentSessionId = UUID.randomUUID

      val registerAgentHelper = new RegisterAgentHelper() {
        def handleListen(response: RegistrationResponse) {
          if (response.agentId != null) {
            println("*************** NewAgentId = " + response.agentId + " ***************")
            println()
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
            println("*************** NewAgentId = " + response.agentId + " ***************")
            println()
            val setContentHelper = new SetContentHelper[Profile] {
              def handleListen(profile: Profile) {
                if (profile != null && profile.firstName.equals("John")) {
                  Results.trigger(resultKey)
                }
              }
            }
            val eventKey = "Set_Profile"
            val target = new AgentCnxnProxy(response.agentId.toString.toURI, "", response.agentId.toString.toURI)
            val profile = new Profile("John", "Smith", "", "", "Canada", "", "", "", "")
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

  "Test" should {
    "Do Stuff" in {
      val resultKey = Results.getKey()
      val agentSessionId = UUID.randomUUID

      // ADD LABEL FOR ALREADY EXISTING AGENT
      val userAgentId = UUID.fromString("1432aa75-b8f6-411c-8ede-7ff4d67ea189")
      def target: AgentCnxnProxy = {
        new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI)
      }

      var connections = new util.ArrayList[Connection]()
      val getContentHelper = new GetContentHelper[Connection]() {
        def handleListen(connection: Connection) {
          connections.add(connection)
        }
      }
      val connectionTag = "Connection" + UUID.randomUUID()
      getContentHelper.listen(ui, agentSessionId, connectionTag)
      getContentHelper.request(ui, agentSessionId, connectionTag, Connection.SEARCH_ALL, target)

      // WAIT FOR CONNECTION TO LOAD
      Thread.sleep(5000)

      val setContentHelper = new SetContentHelper[Label]() {
        def handleListen(data: Label) {
          println("*************** Found CompositeData Data ***************")
          println(data)
        }
      }
      val label = new Label(new LabelKey("profile(name(\"John\"))"), new MessageContent("This is a post"))
      val compositeData = new CompositeData[Label](connections, label)
      val tag = "SetLabel" + UUID.randomUUID()
      setContentHelper.listen(ui, agentSessionId, tag)
      setContentHelper.request(ui, agentSessionId, tag, compositeData, target)

      //Results.triggered(resultKey) must be_==(true).eventually(5, new Duration(2000))
    }
  }


}
