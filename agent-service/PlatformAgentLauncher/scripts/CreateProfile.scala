import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE and UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// CREATE PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("800009c0-e3ba-46f2-87ad-b316668a9f0d")
def target: AgentCnxnProxy = {
  new AgentCnxnProxy(userAgentId.toString.toURI, "", userAgentId.toString.toURI )
}

val profile = new Profile()
profile.setFirstName("Jone")
profile.setLastName("Smith")
profile.setCity("Winnipeg")
profile.setCountry("Canada")

val setContentHelper = new SetContentHelper[Profile]()  {
  def handleListen(profile: Profile) = {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
val tag = "SetProfile" + UUID.randomUUID()
setContentHelper.listen(ui, agentSessionId, tag)
setContentHelper.request(ui, agentSessionId, tag, profile, target)

val getContentHelper = new GetContentHelper[Profile]() {
  def handleListen(profile: Profile) = {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
val tag2 = "GetProfile" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag2)
getContentHelper.request(ui, agentSessionId, tag2, Profile.SEARCH_ALL, target)

