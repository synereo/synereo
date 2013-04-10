import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// GET LABELS FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("2971707f-2d80-4055-8597-d24b21062568")

val getContentPlugin = new GetContentPlugin[Label]()
getContentPlugin.queryObject = Label.SEARCH_ALL
getContentPlugin.run()
