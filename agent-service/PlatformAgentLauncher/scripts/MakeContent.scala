import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("3c0f966f-9b16-47e7-922f-6b3183fffb9f")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val setContentHelper = new SetContentHelper[Label[PostContent]]() {
  def handleListen(label: Label[PostContent]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label = new Label("profile(name)", new Content(new PostContent("This is a post")))
setContentHelper.listen(ui, agentSessionId, "Set_Profile")
setContentHelper.request(ui, agentSessionId, "Set_Profile", label, target)

val setContentHelper2 = new SetContentHelper[Label[LinkContent]]() {
  def handleListen(label: Label[LinkContent]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label2 = new Label("profile(name)",  new Content(new LinkContent("http://www.google.com")))
setContentHelper2.listen(ui, agentSessionId, "Set_Profile")
setContentHelper2.request(ui, agentSessionId, "Set_Profile", label2, target)

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
getContentHelper.listen(ui, agentSessionId, "Get_Label")
getContentHelper.request(ui, agentSessionId, "Get_Label", Label.SEARCH_ALL, target)

