import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("e11c343c-4575-4cf4-8695-7662408a7052")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val getContentHelper = new GetContentHelper[Connection]() {
  def handleListen(connection: Connection) = {
    println("*************** Found Connection Data ***************")
    println(connection)
  }
}

val tag = "GetConnections" + UUID.randomUUID.toString
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Connection.SEARCH_ALL, target)
