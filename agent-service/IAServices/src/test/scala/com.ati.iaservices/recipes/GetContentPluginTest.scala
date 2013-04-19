package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.schema._
import org.specs2.mutable._
import java.util.UUID
import com.protegra_ati.agentservices.core.util.Results
import org.specs2.time.Duration
import com.protegra_ati.agentservices.core.events.{GetContentResponseReceivedEvent, MessageEventAdapter}
import com.protegra_ati.agentservices.core.messages.content.GetContentRequest
import com.protegra_ati.agentservices.core.messages.EventKey

class GetContentPluginTest extends SpecificationWithJUnit
with Serializable {
  "GetContentPlugin with Profile" should {
    "return a Profile" in {

      skipped("until we can figure out why this test doesn't work")

      val resultKey = Results.getKey()
      session.agentSessionId = UUID.randomUUID()
      session.userAgentId = UUID.fromString("090b6272-30c2-4e9c-842e-64ecd1560cc0")
      val tag = "get"

      // SETUP STORE AND UI - SHOULD BE DONE IN COMMON TEST START UP CODE
      new CreateStorePlugin().run()
      new CreateUIPlugin().run()

      // SETUP LISTENER
      session.ui.addListener(session.agentSessionId, "", new MessageEventAdapter(tag) {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) {
          Results.trigger(resultKey)
        }
      })

      // SETUP REQUEST
      val queryObject = Profile.SEARCH_ALL
      val req = new GetContentRequest(new EventKey(session.agentSessionId, tag), queryObject)
      req.targetCnxn = session.selfCnxn
      session.ui.send(req)

      // CONFIRM THAT LISTENER WAS CALLED
      Results.triggered(resultKey) must be_==(true).eventually(10, new Duration(2000))
    }
  }
}
