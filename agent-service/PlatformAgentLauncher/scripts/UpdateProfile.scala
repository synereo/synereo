import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE and UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("5a0660bf-eab6-4c40-9c9f-62e0c6365103")

val getContentPlugin = new GetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
getContentPlugin.queryObject = Profile.SEARCH_ALL
getContentPlugin.run()

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
