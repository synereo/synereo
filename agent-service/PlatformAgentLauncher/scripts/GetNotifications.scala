import com.ati.iaservices.events.{NotificationResponseReceivedEvent}
import com.ati.iaservices.helpers.{GetNotificationResponseHelper, CreateUIHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "c43d8aec-5187-4006-87d4-d876b17d5b85")


val getNotificationHelper = new GetNotificationResponseHelper() {
  def handleListen(e: NotificationResponseReceivedEvent) {
    println("*************** Notification Response Data ***************")
    println(e.toString)
  }
}

val tag = "GetNotification" + UUID.randomUUID()
getNotificationHelper.listen(ui, agentSessionId, tag)
getNotificationHelper.request(ui, agentSessionId, tag, selfCnxn.readCnxn)

