import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// CREATE AN AGENTSESSION
val agentSessionId = UUID.randomUUID

// REGISTER A NEW AGENT
new RegisterAgentPlugin().run()

// WAIT until registerAgent completes

// SET THE PROFILE FOR THE AGENT JUST REGISTERED
val profile = new Profile()
val oldProfile = null
profile.setFirstName("Jane")
profile.setLastName("Doe")
profile.setCountry("Canada")

val setContentPlugin = new SetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
setContentPlugin.listen(agentSessionId, "Set_Profile")
setContentPlugin.request(agentSessionId, "Set_Profile", profile, session.selfCnxn)

// GET THE PROFILE FOR THE AGENT JUST REGISTERED
val getContentPlugin = new GetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
getContentPlugin.listen(agentSessionId, "Get_Profile")
getContentPlugin.request(agentSessionId, "Get_Profile", Profile.SEARCH_ALL, session.selfCnxn)
