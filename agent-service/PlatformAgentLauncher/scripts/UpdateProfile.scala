import com.ati.iaservices.helpers.{CreateUIHelper, CreateStoreHelper, SetContentHelper, GetContentHelper}
import com.protegra_ati.agentservices.core.schema.util.ConnectionFactory
import com.protegra_ati.agentservices.core.schema.Profile
import java.util.UUID

// START STORE and UI PlatformAgents
val store = new CreateStoreHelper().createStore()
val ui = new CreateUIHelper().createUI()

// UPDATE PROFILE FOR ALREADY EXISTING AGENT
val agentSessionId = UUID.randomUUID
val selfCnxn = ConnectionFactory.createSelfConnection("", "58b1d9c0-86bd-4d95-8171-e3696e657754")

def updateProfile(profile: Profile) {
  profile.setFirstName("NewFirst")
  profile.setLastName("NewLast")
  profile.setCountry("NewCountry")

  val setContentHelper = new SetContentHelper[Profile]() {
    def handleListen(profile: Profile) {
      println("*************** Found New Profile Data ***************")
      println(profile)
    }
  }
  val tag = "SetProfile" + UUID.randomUUID()
  setContentHelper.listen(ui, agentSessionId, tag)
  setContentHelper.request(ui, agentSessionId, tag, profile, selfCnxn.writeCnxn)
}

val getContentHelper = new GetContentHelper[Profile]() {
  override def handleListen(profile: Profile) {
    println("*************** Found Profile Data ***************")
    println(profile)
    updateProfile(profile)
  }
}
val tag = "GetProfile" + UUID.randomUUID()
getContentHelper.listen(ui, agentSessionId, tag)
getContentHelper.request(ui, agentSessionId, tag, Profile.SEARCH_ALL, selfCnxn.writeCnxn)


