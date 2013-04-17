import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.{Data, Connection, CompositeData, AgentCnxnProxy}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.ArrayList
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("1432aa75-b8f6-411c-8ede-7ff4d67ea189")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}
// GET ALL CONNECTIONS FOR THE AGENT
var connections = new ArrayList[Connection]()
var getContentHelper = new GetContentHelper[Connection]() {
  def handleListen(connection: Connection) = {
    println("Adding connection: " + connection)
    connections.add(connection)
  }
}
val connectionTag = "Connection" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, connectionTag)
getContentHelper.request(ui, agentSessionId, connectionTag, Connection.SEARCH_ALL, target)

// WAIT FOR CONNECTIONS TO LOAD
Thread.sleep(5000)

val setContentHelper = new SetContentHelper[Label[_ <: ContentValue]]() {
  def handleListen(label: Label[_ <: ContentValue]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}

val tag = "SetLabel" + UUID.randomUUID()
var label : Label[_ <: ContentValue] = new Label(new LabelKey("profile(name(\"Steve\"))"), new Content(new PostContent("This is a post")))
var compositeData = new CompositeData[Label[_ <: ContentValue]](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, compositeData, target)

label = new Label(new LabelKey("profile(name(\"Jane\"))"),  new Content(new LinkContent("http://www.google.com")))
compositeData = new CompositeData[Label[_ <: ContentValue]](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, target)

label = new Label(new LabelKey("profile(address(\"Jane\"))"),  new Content(new PostContent("Winnipeg")))
compositeData = new CompositeData[Label[_ <: ContentValue]](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, target)

label = new Label(new LabelKey("profile(address(\"John\"))"),  new Content(new EmptyContent()))
compositeData = new CompositeData[Label[_ <: ContentValue]](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, target)

val getContentHelper2 = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
val getLabelTag = "GetLabel" + UUID.randomUUID()
getContentHelper2.listen(ui, agentSessionId, getLabelTag)

// GET ALL LABELS
getContentHelper2.request(ui, agentSessionId, getLabelTag, Label.SEARCH_ALL, target)
Thread.sleep(1000)
// GET ALL LABELS
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey(), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(_)"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME)
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(_))"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JOHN))
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(\"John\"))"), null), target)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JANE))
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(\"Jane\"))"), null), target)

