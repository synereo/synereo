import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE and UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("090b6272-30c2-4e9c-842e-64ecd1560cc0")

val getContentPlugin = new GetContentPlugin[Profile]()
getContentPlugin.queryObject = Profile.SEARCH_ALL
getContentPlugin.run()

session.profile.setFirstName("Jane")
session.profile.setLastName("Doe")
session.profile.setCountry("Canada")

//App.saveProfile(App.ui, agentSessionId, agentId, App.profile, oldProfile)

val setContentPlugin = new SetContentPlugin[Profile]
setContentPlugin.data = session.profile
setContentPlugin.oldData = session.oldProfile
setContentPlugin.run()
