import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE and UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("5a0660bf-eab6-4c40-9c9f-62e0c6365103")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val getContentPlugin = new GetContentPlugin[Profile]() {
  def handleListen(profile: Profile) = {
    println("*************** Found Profile Data ***************")
    println(profile)
    updateProfile(profile)
  }
}
getContentPlugin.listen(agentSessionId, "Get_Profile")
getContentPlugin.request(agentSessionId, "Get_Profile", Profile.SEARCH_ALL, target)
getContentPlugin.run()

def updateProfile(profile : Profile) = {
  profile.setFirstName("Jane")
  profile.setLastName("Doe")
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
