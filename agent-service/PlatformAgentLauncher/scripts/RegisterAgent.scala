import com.protegra_ati.platformagentlauncher.App
import java.util.UUID

// START STORE AND UI PlatformAgents
App.initStore()
App.initUI()

// CREATE AN AGENTSESSION
val agentSessionId = UUID.randomUUID()

// REGISTER A NEW AGENT
App.registerAgent(App.ui, agentSessionId, "John Smith")

// WAIT until registerAgent completes

// GET THE PROFILE FOR THE AGENT JUST REGISTERED
App.getProfile(App.ui, agentSessionId, App.lastCreatedAgentId)

