import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID
import scala.collection.mutable

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// GET LABELS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "05f4482a-c4cc-4ddd-a07a-fcfb2e50700f")

var allLabels = new mutable.MutableList[String]

val getContentHelper = new GetContentHelper[Label]() {
  override def handleListen(label: Label) {
    allLabels += label.key.searchKey
  }
}
val tag = "GetLabel" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Label.SEARCH_ALL, selfCnxn.writeCnxn)

Thread.sleep(2000)
val all = "All Labels (" + allLabels.size + ") : " + allLabels.mkString("", ", ", "")
val unique = "Unique Labels (" + Set(allLabels.toArray: _*).size + ") : " + Set(allLabels.toArray: _*).mkString("", ", ", "")
all
unique
