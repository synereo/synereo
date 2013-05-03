package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import com.protegra_ati.agentservices.core.messages.EventKey
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.core.util.Results
import java.util.UUID
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.time.Duration
import com.ati.iaservices.events.MessageFactory

class GetContentPluginTest extends SpecificationWithJUnit
with Serializable {
  val retries = 10
  val duration = 2000

  "GetContentPlugin with Profile" should {
    "return a Profile" in {

      skipped("until we can figure out why this test doesn't work")

      val resultKey = Results.getKey()
      session.agentSessionId = UUID.randomUUID()
      session.userAgentId = UUID.fromString("090b6272-30c2-4e9c-842e-64ecd1560cc0")
      val tag = "get"

      // SETUP STORE AND UI - SHOULD BE DONE IN COMMON TEST START UP CODE
      new CreateStorePlugin().run()
      new CreateDSLPlugin().run()

      // SETUP LISTENER
      session.dsl.addListener(session.agentSessionId, "", new MessageEventAdapter(tag) {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
          Results.trigger(resultKey)
        }
      })

      // SETUP REQUEST
      val req = MessageFactory.createGetContentRequest(session.agentSessionId, tag, Profile.SEARCH_ALL, session.selfCnxn)
      session.dsl.send(req)

      // CONFIRM THAT LISTENER WAS CALLED
      Results.triggered(resultKey) must be_==(true).eventually(retries, new Duration(duration))
    }
  }
}
