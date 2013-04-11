import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET LABELS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("3c0f966f-9b16-47e7-922f-6b3183fffb9f")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
getContentHelper.listen(ui, agentSessionId, "Get_Label")
getContentHelper.request(ui, agentSessionId, "Get_Label", Label.SEARCH_ALL, target)
