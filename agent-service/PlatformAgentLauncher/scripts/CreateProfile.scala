import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.protegra_ati.agentservices.core.schema.{AgentCnxnProxy, Profile}
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID

// START STORE and UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// CREATE PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val userAgentId = UUID.fromString("5a0660bf-eab6-4c40-9c9f-62e0c6365103")
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
setContentHelper.listen(ui, agentSessionId, "Set_Profile")
setContentHelper.request(ui, agentSessionId, "Get_Profile", profile, target)

val getContentHelper = new GetContentHelper[Profile]() {
  def handleListen(profile: Profile) = {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
getContentHelper.listen(ui, agentSessionId, "Get_Profile")
getContentHelper.request(ui, agentSessionId, "Get_Profile", Profile.SEARCH_ALL, target)

