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
val userAgentId = UUID.fromString("800009c0-e3ba-46f2-87ad-b316668a9f0d")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
val tag = "GetLabel" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Label.SEARCH_ALL, target)

// REPLACEMENT FOR getContentHelper.request
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import com.protegra_ati.agentservices.core.messages.EventKey
val eventKey: EventKey = new EventKey(agentSessionId, tag)
val msg: GetContentRequest = new GetContentRequest(eventKey, Label.SEARCH_ALL)
msg.setTargetCnxn(target)
ui.send(msg)