import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, GetContentHelper, SetContentHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE and UI PlatformAgents
val store = new CreateStoreHelper().createStore
val ui = new CreateUIHelper().createUI

// CREATE NEW PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "58b1d9c0-86bd-4d95-8171-e3696e657754")

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
setContentHelper.request(ui, agentSessionId, tag, profile, selfCnxn.writeCnxn)

// GET THE PROFILE(S) FOR THE AGENT
val getContentHelper = new GetContentHelper[Profile]() {
  def handleListen(profile: Profile) = {
    println("*************** Found Profile Data ***************")
    println(profile)
  }
}
val tag2 = "GetProfile" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag2)
getContentHelper.request(ui, agentSessionId, tag2, Profile.SEARCH_ALL, selfCnxn.writeCnxn)

