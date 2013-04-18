import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID
import scala.collection.mutable

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// GET LABELS FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "58b1d9c0-86bd-4d95-8171-e3696e657754")

var allLabels = new mutable.MutableList[String]

val getContentHelper = new GetContentHelper[Label[_]]() {
  def handleListen(label: Label[_]) = {
    allLabels += label.key.getSearchKey()
  }
}
val tag = "GetLabel" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Label.SEARCH_ALL, selfCnxn.writeCnxn)

Thread.sleep(2000)
val all = "All Labels (" + allLabels.size + ") : " + allLabels.mkString("", ", ", "")
val unique = "Unique Labels (" + Set(allLabels.toArray : _*).size + ") : " + Set(allLabels.toArray : _*).mkString("", ", ", "")
all
unique
