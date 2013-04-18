import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Connection
import java.util.{ArrayList, UUID}

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET LABELS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "58b1d9c0-86bd-4d95-8171-e3696e657754")

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
getContentHelper.request(ui, agentSessionId, connectionTag, Connection.SEARCH_ALL, selfCnxn.writeCnxn)

// WAIT FOR CONNECTIONS TO LOAD
Thread.sleep(5000)

val getContentHelper2 = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
val tag = "GetLabel" + UUID.randomUUID()
getContentHelper2.listen(ui, agentSessionId, tag)
getContentHelper2.request(ui, agentSessionId, tag, Label.SEARCH_ALL, connections.get(0).readCnxn)

// REPLACEMENT FOR getContentHelper.request
//import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
//import com.protegra_ati.agentservices.core.messages.EventKey
//val eventKey: EventKey = new EventKey(agentSessionId, tag)
//val msg: GetContentRequest = new GetContentRequest(eventKey, Label.SEARCH_ALL)
//msg.setTargetCnxn(selfCnxn.writeCnxn)
//ui.send(msg)