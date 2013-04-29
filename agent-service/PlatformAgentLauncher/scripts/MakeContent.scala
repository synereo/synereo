import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.{Connection, CompositeData}
import java.util
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "c7c7c097-2942-472e-9962-7066ae153c6b")

// GET ALL CONNECTIONS FOR THE AGENT
var connections = new util.ArrayList[Connection]()
var getContentHelper = new GetContentHelper[Connection]() {
  override def handleListen(connection: Connection) {
    println("Adding connection: " + connection)
    connections.add(connection)
  }
}
val connectionTag = "Connection" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, connectionTag)
getContentHelper.request(ui, agentSessionId, connectionTag, Connection.SEARCH_ALL, selfCnxn.writeCnxn)

// WAIT FOR CONNECTIONS TO LOAD
Thread.sleep(5000)

val setContentHelper = new SetContentHelper[Label]() {
  def handleListen(label: Label) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}

val tag = "SetLabel" + UUID.randomUUID()
var label = new Label(new LabelKey("profile(name(\"Steve\"))"), new MessageContent("This is a post"))
var compositeData = new CompositeData[Label](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, compositeData, selfCnxn.writeCnxn)

label = new Label(new LabelKey("profile(name(\"Jane\"))"), new ImageContent("http://www.google.com", "Title"))
compositeData = new CompositeData[Label](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, selfCnxn.writeCnxn)

label = new Label(new LabelKey("profile(address(\"Jane\"))"), new MessageContent("Winnipeg"))
compositeData = new CompositeData[Label](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, selfCnxn.writeCnxn)

label = new Label(new LabelKey("profile(address(\"John\"))"), new EmptyContent())
compositeData = new CompositeData[Label](connections, label)
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, label, selfCnxn.writeCnxn)

val getContentHelper2 = new GetContentHelper[Label]() {
  override def handleListen(label: Label) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
val getLabelTag = "GetLabel" + UUID.randomUUID()
getContentHelper2.listen(ui, agentSessionId, getLabelTag)

// GET ALL LABELS
getContentHelper2.request(ui, agentSessionId, getLabelTag, Label.SEARCH_ALL, selfCnxn.writeCnxn)
Thread.sleep(1000)
// GET ALL LABELS
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey(), null), selfCnxn.writeCnxn)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(_)"), null), selfCnxn.writeCnxn)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME)
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(_))"), null), selfCnxn.writeCnxn)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JOHN))
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(\"John\"))"), null), selfCnxn.writeCnxn)
Thread.sleep(1000)
// GET ALL LABELS WITH PROFILE(NAME(JANE))
getContentHelper2.request(ui, agentSessionId, getLabelTag, new Label(new LabelKey("profile(name(\"Jane\"))"), null), selfCnxn.writeCnxn)

