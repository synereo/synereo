import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.{Data, Connection}
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "f5bc533a-d417-4d71-ad94-8c766907381b")
var count = 0

val getContentHelper = new GetContentHelper[Connection]() {
  override def handleListen(connection: Connection) {
    count = count + 1
    println("*************** Found Connection " + count + " ***************")
    println(connection)
  }
  override def handleListen(connections: List[Data]) {
    println("*************** Found Connections ***************")
    println(connections)
  }
}

val tag = "GetConnections" + UUID.randomUUID.toString
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Connection.SEARCH_ALL, selfCnxn.writeCnxn)
