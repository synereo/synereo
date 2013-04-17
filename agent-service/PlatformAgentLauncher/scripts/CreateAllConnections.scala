import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, ConnectToAllHelper}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("c489c71c-fd6c-46b5-bc4d-d9267f31520b")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val connectToAllHelper = new ConnectToAllHelper() {
  def handleConnectionsCompleted() = {
    println("*************** Connect To All Completed ***************")
  }
}

connectToAllHelper.connectToAll(ui, target, agentSessionId, "John Smith1", userAgentId)
