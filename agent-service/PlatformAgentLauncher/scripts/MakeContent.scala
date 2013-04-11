import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// ADD LABEL FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("3c0f966f-9b16-47e7-922f-6b3183fffb9f")

val setContentPlugin = new SetContentPlugin[Label[PostContent]]() {
  def handleListen(label: Label[PostContent]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label = new Label("Profile(Name)", new Content(new PostContent("This is a post")))
setContentPlugin.data = label
setContentPlugin.oldData = null
setContentPlugin.run()

val setContentPlugin2 = new SetContentPlugin[Label[LinkContent]]() {
  def handleListen(label: Label[LinkContent]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label2 = new Label("Profile(Name)",  new Content(new LinkContent("http://www.google.com")))
setContentPlugin2.data = label2
setContentPlugin2.oldData = null
setContentPlugin2.run()

val getContentPlugin = new GetContentPlugin[Label[_]]() {
  def handleListen(label: Label[_]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
getContentPlugin.queryObject = Label.SEARCH_ALL
getContentPlugin.run()

