import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Connection
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "d8da6610-4515-439b-9ebd-bd91badcb6b1")

val getContentHelper = new GetContentHelper[Connection]() {
  def handleListen(connection: Connection) {
    println("*************** Found Connection Data ***************")
    println(connection)
  }
}

val tag = "GetConnections" + UUID.randomUUID.toString
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Connection.SEARCH_ALL, selfCnxn.writeCnxn)
