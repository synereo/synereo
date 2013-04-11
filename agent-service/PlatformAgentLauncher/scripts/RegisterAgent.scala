import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.messages.admin.RegistrationResponse
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// CREATE AN AGENTSESSION
val agentSessionId = UUID.randomUUID
val BIZNETWORK_AGENT_ID = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")

def createProfile(agentId: UUID) = {
  def target: AgentCnxnProxy = {
    new AgentCnxnProxy(agentId.toString.toURI, "", agentId.toString.toURI )
  }

  val profile = new Profile()
  val oldProfile = null
  profile.setFirstName("John")
  profile.setLastName("Smith")
  profile.setCity("Winnipeg")
  profile.setCountry("Canada")

  val setContentPlugin = new SetContentPlugin[Profile]() {
    def handleListen(profile: Profile) = {
      println("*************** Found Profile Data ***************")
      println(profile)
    }
  }
  setContentPlugin.listen(agentSessionId, "Set_Profile")
  setContentPlugin.request(agentSessionId, "Set_Profile", profile, target)
}

// REGISTER A NEW AGENT
val registerAgentPlugin = new RegisterAgentPlugin() {
  def handleListen(response: RegistrationResponse) = {
    println("*************** Found RegistrationResponse Data ***************")
    println(response)
    println("*************** New AgentId = " + response.agentId + " ***************")
    createProfile(response.agentId)
  }
}
registerAgentPlugin.listen(agentSessionId, "Register")
registerAgentPlugin.request(agentSessionId, "Register", BIZNETWORK_AGENT_ID, "John Smith")

