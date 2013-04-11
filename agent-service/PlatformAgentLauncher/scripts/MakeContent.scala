import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("3c0f966f-9b16-47e7-922f-6b3183fffb9f")

val setContentPlugin = new SetContentPlugin[Label[PostContent]]() {
  def handleListen(label: Label[PostContent]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label = new Label("Profile(Name)", new Content(new PostContent("This is a post")))
setContentPlugin.listen(agentSessionId, "Set_Profile")
setContentPlugin.request(agentSessionId, "Set_Profile", label, session.selfCnxn)

val setContentPlugin2 = new SetContentPlugin[Label[LinkContent]]() {
  def handleListen(label: Label[LinkContent]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label2 = new Label("Profile(Name)",  new Content(new LinkContent("http://www.google.com")))
setContentPlugin2.listen(agentSessionId, "Set_Profile")
setContentPlugin2.request(agentSessionId, "Set_Profile", label2, session.selfCnxn)

val getContentPlugin = new GetContentPlugin[Label[_]]() {
  def handleListen(label: Label[_]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
getContentPlugin.listen(agentSessionId, "Get_Label")
getContentPlugin.request(agentSessionId, "Get_Label", Label.SEARCH_ALL, session.selfCnxn)

