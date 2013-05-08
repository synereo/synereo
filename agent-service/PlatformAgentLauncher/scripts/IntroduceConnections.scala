import com.ati.iaservices.events.{CreateReferralResponseReceivedEvent}
import com.ati.iaservices.helpers.{CreateDSLHelper, CreateStoreHelper, CreateReferralHelper}
import com.ati.iaservices.schema._
import com.ati.iaservices.messages.referral.Referral
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Post
import java.util.UUID

// START STORE AND UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val dsl = new CreateDSLHelper().createDSL()

// ADD LABEL FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "f5bc533a-d417-4d71-ad94-8c766907381b")

val createReferralHelper = new CreateReferralHelper() {
  def handleListen(e: CreateReferralResponseReceivedEvent) {
    println("*************** Create Referral Response Data ***************")
    println(e.toString)
  }
}

val tag = "CreateReferral" + UUID.randomUUID()
val referral_A = new Referral(UUID.fromString("820852a6-2150-432e-b073-df8df660ca51"), "John Smith", new Post())
val referral_B = new Referral(UUID.fromString("78fe64df-5984-4c7e-b8d2-e09b3c345be1"), "John Smith", new Post())
createReferralHelper.listen(dsl, agentSessionId, tag)
createReferralHelper.request(dsl, agentSessionId, tag, selfCnxn.readCnxn)(referral_A)(referral_B)

