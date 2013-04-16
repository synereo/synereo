import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.AgentCnxnProxy
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import scala.collection.mutable

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET LABELS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("1432aa75-b8f6-411c-8ede-7ff4d67ea189")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

var allLabels = new mutable.MutableList[String]

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    allLabels += label.key.getSearchKey()
  }
}
val tag = "GetLabel" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Label.SEARCH_ALL, target)

Thread.sleep(2000)
val all = "All Labels (" + allLabels.size + ") : " + allLabels.mkString("", ", ", "")
val unique = "Unique Labels (" + Set(allLabels.toArray : _*).size + ") : " + Set(allLabels.toArray : _*).mkString("", ", ", "")
all
unique
