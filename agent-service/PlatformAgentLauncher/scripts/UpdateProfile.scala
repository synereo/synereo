import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE and UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("5a0660bf-eab6-4c40-9c9f-62e0c6365103")

val getContentPlugin = new GetContentPlugin[Profile]() {
  def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
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
  }
}
getContentPlugin.listen(agentSessionId, "Get_Profile")
getContentPlugin.request(agentSessionId, "Get_Profile", Profile.SEARCH_ALL, session.selfCnxn)
getContentPlugin.run()

