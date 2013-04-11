import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// CREATE AN AGENTSESSION
session.agentSessionId = UUID.randomUUID

// REGISTER A NEW AGENT
new RegisterAgentPlugin().run()

// WAIT until registerAgent completes

// SET THE PROFILE FOR THE AGENT JUST REGISTERED
session.profile = new Profile()
session.oldProfile = null
session.profile.setFirstName("Jane")
session.profile.setLastName("Doe")
session.profile.setCountry("Canada")

val setContentPlugin = new SetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
setContentPlugin.data = session.profile
setContentPlugin.oldData = session.oldProfile
setContentPlugin.run()

// GET THE PROFILE FOR THE AGENT JUST REGISTERED
val getContentPlugin = new GetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
getContentPlugin.queryObject = Profile.SEARCH_ALL
getContentPlugin.run()
