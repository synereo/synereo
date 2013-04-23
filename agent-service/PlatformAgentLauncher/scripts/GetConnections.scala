import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Connection
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "84114eba-9214-4623-b3ae-f247ee779572")
var count = 0

val getContentHelper = new GetContentHelper[Connection]() {
  def handleListen(connection: Connection) {
    count = count + 1
    println("*************** Found Connection " + count + " ***************")
    println(connection)
  }
}

val tag = "GetConnections" + UUID.randomUUID.toString
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Connection.SEARCH_ALL, selfCnxn.writeCnxn)
