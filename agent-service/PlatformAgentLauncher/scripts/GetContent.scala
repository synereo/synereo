import com.ati.iaservices.recipes.LauncherPluginSession._
import com.ati.iaservices.recipes._
import com.ati.iaservices.schema._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// GET LABELS FOR ALREADY EXISTING AGENT
session.agentSessionId = UUID.randomUUID
session.userAgentId = UUID.fromString("3c0f966f-9b16-47e7-922f-6b3183fffb9f")

val getContentPlugin = new GetContentPlugin[Label[_]]() {
  override def handleListen(label: Label[_]) {
    println("*************** Found Label Data ***************")
    println(label)
  }
}
getContentPlugin.listen(session.agentSessionId, "Get_Label")
getContentPlugin.request(session.agentSessionId, "Get_Label", Label.SEARCH_ALL, session.selfCnxn)
