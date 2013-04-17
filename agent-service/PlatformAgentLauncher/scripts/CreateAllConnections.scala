import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, ConnectToAllHelper}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("1c0c3b17-3b73-434b-949d-4e492fb6c76b")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val connectToAllHelper = new ConnectToAllHelper() {
  def handleConnectionsCompleted() = {
    println("*************** Connect To All Completed ***************")
  }
}

connectToAllHelper.connectToAll(ui, target, agentSessionId, "John Smith1", userAgentId)
