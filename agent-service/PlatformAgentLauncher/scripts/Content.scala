import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// ADD LABEL FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("aa031f2b-115b-47bd-90de-ce90caabdc4e")

val setContentPlugin = new SetContentPlugin[Label[PostContent]]()
var label = new Label("Profile(Name)", new Content(new PostContent("This is a post")))
setContentPlugin.data = label
setContentPlugin.oldData = null
setContentPlugin.run()

val setContentPlugin2 = new SetContentPlugin[Label[LinkContent]]()
var label2 = new Label("Profile(Name)",  new Content(new LinkContent("http://www.google.com")))
setContentPlugin2.data = label2
setContentPlugin2.oldData = null
setContentPlugin2.run()

val getContentPlugin = new GetContentPlugin[Label[_]]()
getContentPlugin.queryObject = Label.SEARCH_ALL
getContentPlugin.run()

