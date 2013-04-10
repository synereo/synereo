import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// ADD LABEL FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("2971707f-2d80-4055-8597-d24b21062568")

val setContentPlugin = new SetContentPlugin[Label]()
var label = new Label("Root Label", "")
setContentPlugin.data = label
setContentPlugin.oldData = new Label()
setContentPlugin.run()

label = new Label("Level 1 Child", session.label.id) // session.label is the label returned from the last SetContentRequest
setContentPlugin.data = label
setContentPlugin.oldData = new Label()
setContentPlugin.run()

label = new Label("Level 2 Child", session.label.id) // session.label is the label returned from the last SetContentRequest
setContentPlugin.data = label
setContentPlugin.oldData = new Label()
setContentPlugin.run()

// UPDATE LAST LABEL
session.label.name = "Level 2 Child - New Name"
setContentPlugin.data = session.label
setContentPlugin.oldData = session.oldLabel
setContentPlugin.run()

