import com.ati.iaservices.events.CreateReferralResponseReceivedEvent
import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, CreateReferralHelper}
import com.ati.iaservices.schema._
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Post
import java.util
import java.util.UUID

// START STORE AND UI PlatformAgents
//val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "c43d8aec-5187-4006-87d4-d876b17d5b85")

val createReferralHelper = new CreateReferralHelper() {
  def handleListen(e: CreateReferralResponseReceivedEvent) {
    println("*************** Create Referral Response Data ***************")
    println(e.toString)
  }
}

val tag = "CreateReferral" + UUID.randomUUID()
createReferralHelper.listen(ui, agentSessionId, tag)
createReferralHelper.request(ui, agentSessionId, tag, target)
                            (UUID.fromString("c43d8aec-5187-4006-87d4-d876b17d5b85"), "John Smith", new Post() )
                            (UUID.fromString("5a40a174-3c4d-48a4-a609-4d62680f55e5"), "John Smith", new Post() )

