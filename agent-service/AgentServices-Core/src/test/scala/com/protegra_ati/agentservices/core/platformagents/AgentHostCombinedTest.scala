package com.protegra_ati.agentservices.core.platformagents

import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import org.junit._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import java.net.URI
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import invitation._
import scala.util.Random

import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import java.util.{Locale, UUID}
import Assert._

class AgentHostCombinedTest extends SpecificationWithJUnit
with Timeouts
{
  val cnxnUIStore = new AgentCnxnProxy(( "UI" + UUID.randomUUID().toString ).toURI, "", ( "Store" + UUID.randomUUID().toString ).toURI)
  val storeR = new AgentHostStorePlatformAgent
  val uiR = new AgentHostUIPlatformAgent
  AgentHostCombinedBase.setupPAs(storeR, uiR, cnxnUIStore)

  val jenUID = UUID.randomUUID()
  var cnxnJenJen = new AgentCnxnProxy(( "Jen" + jenUID.toString ).toURI, "CombinedTestUser", ( "Jen" + jenUID.toString ).toURI)

  val mikeUID = UUID.randomUUID()
  var cnxnMikeMike = new AgentCnxnProxy(( "Mike" + mikeUID.toString ).toURI, "CombinedTestUser", ( "Mike" + mikeUID.toString ).toURI)
//  storeR._cnxnUserSelfConnectionsList = List(cnxnJenJen, cnxnMikeMike)
  //  as soon as an new connection is stored, (esp. self connection) it is necessary to listen to this connection or like here to ALL selfconnections
  //  If no listening happened than connection as if would be "not visible"
//  storeR.listenForHostedCnxns()

  "SetGet" should {
    "retrieve english Profile between UI and Store with a public queue" in {
      val agentSessionId = UUID.randomUUID()
      val eventKey = "content"

      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey, Locale.ENGLISH.toString())
      AgentHostCombinedBase.countProfile(uiR, cnxnJenJen, agentSessionId, eventKey, None) must be_==(1).eventually(3, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
    }
  }

}
