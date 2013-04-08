import com.protegra_ati.platformagentlauncher.App
import com.rits.cloning.Cloner
import java.util.UUID

// START STORE and UI PlatformAgents
App.initStore()
App.initUI()

// CREATE AN AGENTSESSION
val agentSessionId = UUID.randomUUID()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
val agentId = UUID.fromString("090b6272-30c2-4e9c-842e-64ecd1560cc0")
App.getProfile(App.ui, agentSessionId, agentId)
val oldProfile = new Cloner().deepClone( App.profile )

App.profile.setFirstName("John")
App.profile.setLastName("Smith")
App.profile.setCountry("Canada")

App.saveProfile(App.ui, agentSessionId, agentId, App.profile, oldProfile)
