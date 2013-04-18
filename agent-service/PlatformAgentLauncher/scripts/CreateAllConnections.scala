import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, ConnectToAllHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "58b1d9c0-86bd-4d95-8171-e3696e657754")

val connectToAllHelper = new ConnectToAllHelper() {
  def handleConnectionsCompleted() = {
    println("*************** Connect To All Completed ***************")
  }
}

connectToAllHelper.connectToAll(ui, selfCnxn.writeCnxn, agentSessionId, "John Smith1")
