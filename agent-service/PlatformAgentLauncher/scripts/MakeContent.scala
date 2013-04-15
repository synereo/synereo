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
val userAgentId = UUID.fromString("f2158eaa-60af-4c74-913e-0159d5f0a43b")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val setContentHelper = new SetContentHelper[Label[PostContent]]() {
  def handleListen(label: Label[PostContent]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label = new Label(new LabelKey("profile(name(\"John\"))"), new Content(new PostContent("This is a post")))
val tag = "SetLabel" + UUID.randomUUID()
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, target)

val setContentHelper2 = new SetContentHelper[Label[LinkContent]]() {
  def handleListen(label: Label[LinkContent]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label2 = new Label(new LabelKey("profile(name(\"Jane\"))"),  new Content(new LinkContent("http://www.google.com")))
val tag2 = "SetLabel" + UUID.randomUUID()
setContentHelper2.listen(ui, agentSessionId, tag2)
setContentHelper2.request(ui, agentSessionId, tag2, label2, target)

val setContentHelper3 = new SetContentHelper[Label[PostContent]]() {
  def handleListen(label: Label[PostContent]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
var label3 = new Label(new LabelKey("profile(address(\"Jane\"))"),  new Content(new PostContent("Winnipeg")))
val tag3 = "SetLabel" + UUID.randomUUID()
setContentHelper3.listen(ui, agentSessionId, tag3)
setContentHelper3.request(ui, agentSessionId, tag3, label3, target)

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
val tag4 = "GetLabel" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag4)

// GET ALL LABELS
getContentHelper.request(ui, agentSessionId, tag4, Label.SEARCH_ALL, target)
Thread.sleep(1000)
// GET ALL LABELS
getContentHelper.request(ui, agentSessionId, tag4, new Label(new LabelKey(), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE
getContentHelper.request(ui, agentSessionId, tag4, new Label(new LabelKey("profile(_)"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME)
getContentHelper.request(ui, agentSessionId, tag4, new Label(new LabelKey("profile(name(_))"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JOHN))
getContentHelper.request(ui, agentSessionId, tag4, new Label(new LabelKey("profile(name(\"John\"))"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JANE))
getContentHelper.request(ui, agentSessionId, tag4, new Label(new LabelKey("profile(name(\"Jane\"))"), null), target)

