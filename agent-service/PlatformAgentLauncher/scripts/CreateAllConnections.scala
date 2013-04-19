import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, ConnectToAllHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "29486766-1d82-4c47-93cd-21624b052cdd")

val connectToAllHelper = new ConnectToAllHelper() {
  def handleConnectionsCompleted() {
    println("*************** Connect To All Completed ***************")
  }
}

connectToAllHelper.connectToAll(ui, selfCnxn.writeCnxn, agentSessionId, "John Smith")
