import com.ati.iaservices.helpers.{RegisterAgentHelper, CreateUIHelper, CreateStoreHelper, SetContentHelper}
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// CREATE AN AGENTSESSION
val agentSessionId = UUID.randomUUID
val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")

def createProfile(agentId: UUID) = {
  def target: AgentCnxnProxy = {
    new AgentCnxnProxy(agentId.toString.toURI, "", agentId.toString.toURI )
  }

  val profile = new Profile()
  profile.setFirstName("John")
  profile.setLastName("Smith")
  profile.setCity("Winnipeg")
  profile.setCountry("Canada")

  val setContentHelper = new SetContentHelper[Profile]() {
    def handleListen(profile: Profile) = {
      println("*************** Found Profile Data ***************")
      println(profile)
    }
  }
  val tag = "SetProfile" + UUID.randomUUID()
  setContentHelper.listen(ui, agentSessionId, tag)
  setContentHelper.request(ui, agentSessionId, tag, profile, target)
}

// REGISTER A NEW AGENT
val registerAgentHelper = new RegisterAgentHelper() {
  def handleListen(response: RegistrationResponse) = {
    println("*************** Found RegistrationResponse Data ***************")
    println(response)
    println("*************** New AgentId = " + response.agentId + " ***************")
    createProfile(response.agentId)
  }
}
val tag = "Register" + UUID.randomUUID()
registerAgentHelper.listen(ui, agentSessionId, tag)
registerAgentHelper.request(ui, agentSessionId, tag, BIZNETWORK_AGENT_ID, "John Smith")

