import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, ConnectToAllHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "373cbb00-8d04-4723-88f2-550b52ea46b7")

val connectToAllHelper = new ConnectToAllHelper() {
  def handleConnectionsCompleted() {
    println("*************** Connect To All Completed ***************")
  }
}

connectToAllHelper.connectToAll(ui, selfCnxn.writeCnxn, agentSessionId, "John Smith")
