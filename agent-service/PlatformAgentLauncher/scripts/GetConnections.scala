import com.ati.iaservices.recipes._
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Connection}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE AND UI PlatformAgents
new CreateStorePlugin().run()
new CreateUIPlugin().run()

// GET CONNECTIONS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("f5bc533a-d417-4d71-ad94-8c766907381b")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val getContentPlugin = new GetContentPlugin[Connection]() {
  def handleListen(connection: Connection) = {
    println("*************** Found Connection Data ***************")
    println(connection)
  }
}
getContentPlugin.listen(agentSessionId, "Get_Connection")
getContentPlugin.request(agentSessionId, "Get_Connection", Connection.SEARCH_ALL, target)
