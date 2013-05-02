package com.ati.iaservices.helpers

import com.protegra_ati.agentservices.core.util.Results
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.schema.{CompositeData, Connection, AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.ati.iaservices.schema.{LabelKey, MessageContent, Label}
import java.util
import java.util.UUID
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.time.Duration

// scalastyle:off regex

class HelperTest extends SpecificationWithJUnit
with Serializable {
  val retries = 10
  val timeoutDuration = 2000
  val firstName = "John"
  val lastName = "Smith"
  val alias = firstName + " " + lastName
  val newAgentIdFormatString = "*************** NewAgentId = %s ***************"
  val tag = "Register"
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
              println(String.format(newAgentIdFormatString, response.agentId))
              println()
              Results.trigger(resultKey)
            }
          }
        }
        val eventKey = tag + UUID.randomUUID().toString
        registerAgentHelper.listen(ui, agentSessionId, eventKey)
        registerAgentHelper.request(ui, agentSessionId, eventKey, BIZNETWORK_AGENT_ID, alias)

        Results.triggered(resultKey) must be_==(true).eventually(retries, new Duration(timeoutDuration))
      }
    }

    "SetContentHelper with Profile" should {
      "Create a profile" in {
        val resultKey = Results.getKey()
        val agentSessionId = UUID.randomUUID

        val registerAgentHelper = new RegisterAgentHelper() {
          def handleListen(response: RegistrationResponse) {
            if (response.agentId != null) {
              println(String.format(newAgentIdFormatString, response.agentId))
              println()
              val setContentHelper = new SetContentHelper[Profile] {
                def handleListen(profile: Profile) {
                  if (profile != null && profile.firstName.equals(firstName)) {
                    Results.trigger(resultKey)
                  }
                }
              }
              val eventKey = "Set_Profile"
              val target = new AgentCnxnProxy(response.agentId.toString.toURI, "", response.agentId.toString.toURI)
              val profile = new Profile(firstName, lastName, "", "", "", "", "", "", "")
              setContentHelper.listen(ui, agentSessionId, eventKey)
              setContentHelper.request(ui, agentSessionId, eventKey, profile, target)
            }
          }
        }
        val eventKey = tag + UUID.randomUUID().toString
        registerAgentHelper.listen(ui, agentSessionId, eventKey)
        registerAgentHelper.request(ui, agentSessionId, eventKey, BIZNETWORK_AGENT_ID, alias)

        Results.triggered(resultKey) must be_==(true).eventually(retries, new Duration(timeoutDuration))
      }
    }

  "ConnectToAllHelper" should {
    "Create connection to everyone connected to source agent (BIZNETWORK_AGENT_ID)" in {
      val resultKey1 = Results.getKey()
      val resultKey2 = Results.getKey()

      val registerAgentHelper = new RegisterAgentHelper() {
        def handleListen(response: RegistrationResponse) {
          if (response.agentId != null) {
            println(String.format(newAgentIdFormatString, response.agentId))
            println()

            val connectToAllHelper = new ConnectToAllHelper {
              def handleConnectionsCompleted() {

                def countConnections(agentId: UUID, resultKey: String) {
                  var count = 0
                  val getContentHelper = new GetContentHelper[Connection]() {
                    override def handleListen(connection: Connection) {
                      count = count + 1
                      Results.count(resultKey, count)
                    }
                  }

                  val tag = "GetConnections" + UUID.randomUUID.toString
                  val agentSessionId = UUID.randomUUID
                  getContentHelper.listen(ui, agentSessionId, tag)
                  getContentHelper.request(ui, agentSessionId, tag, Connection.SEARCH_ALL, ConnectionFactory.createSelfConnection("", agentId.toString).writeCnxn)
                }

                countConnections(response.agentId, resultKey1)
                countConnections(BIZNETWORK_AGENT_ID, resultKey2)
              }
            }
            val agentSessionId = UUID.randomUUID
            connectToAllHelper.connectToAll(ui, ConnectionFactory.createSelfConnection(alias, response.agentId.toString).writeCnxn, agentSessionId, alias, BIZNETWORK_AGENT_ID)
          }
        }
      }
      val eventKey = tag + UUID.randomUUID().toString
      val agentSessionId = UUID.randomUUID
      registerAgentHelper.listen(ui, agentSessionId, eventKey)
      registerAgentHelper.request(ui, agentSessionId, eventKey, BIZNETWORK_AGENT_ID, alias)

      // Confirm that number of BIZ connections equals number of new agent connections
      Results.counted(resultKey1) must be_==(Results.counted(resultKey2)).eventually(retries, new Duration(timeoutDuration))
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
        override def handleListen(connection: Connection) {
          connections.add(connection)
        }
      }
      val connectionTag = "Connection" + UUID.randomUUID()
      getContentHelper.listen(ui, agentSessionId, connectionTag)
      getContentHelper.request(ui, agentSessionId, connectionTag, Connection.SEARCH_ALL, target)

      // WAIT FOR CONNECTION TO LOAD
      Thread.sleep(timeoutDuration)

      val setContentHelper = new SetContentHelper[Label]() {
        def handleListen(data: Label) {
          println("*************** Found Label Data ***************")
          println(data)
        }
      }
      val label = new Label(new LabelKey("profile(name(\"John\"))"), new MessageContent("This is a post"))
      val compositeData = new CompositeData[Label](connections, label)
      val tag = "SetLabel" + UUID.randomUUID()
      setContentHelper.listen(ui, agentSessionId, tag)
      setContentHelper.request(ui, agentSessionId, tag, compositeData, target)

      //Results.triggered(resultKey) must be_==(true).eventually(retries, new Duration(timeoutDuration))
    }
  }
}
