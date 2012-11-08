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

import org.specs._
import org.specs.Specification
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.events._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._
import java.util.{Locale, UUID}
import Assert._

class AgentHostCombinedTest
  extends JUnit4(AgentHostCombinedTestSpecs)

object AgentHostCombinedTestSpecsRunner
  extends ConsoleRunner(AgentHostCombinedTestSpecs)

object AgentHostCombinedTestSpecs extends Specification
with Timeouts
{

  AgentHostCombinedBase.setup(this)
  val uiR = AgentHostCombinedBase.uiRef
  val storeR = AgentHostCombinedBase.storeRef
  val jenUID = UUID.randomUUID()
  var cnxnJenJen = new AgentCnxnProxy(( "Jen" + jenUID.toString ).toURI, "CombinedTestUser", ( "Jen" + jenUID.toString ).toURI)

  val mikeUID = UUID.randomUUID()
  var cnxnMikeMike = new AgentCnxnProxy(( "Mike" + mikeUID.toString ).toURI, "CombinedTestUser", ( "Mike" + mikeUID.toString ).toURI)
  storeR._cnxnUserSelfConnectionsList = List(cnxnJenJen, cnxnMikeMike)
  //  as soon as an new connection is stored, (esp. self connection) it is necessary to listen to this connection or like here to ALL selfconnections
  //  If no listening happened than connection as if would be "not visible"
  storeR.listenForHostedCnxns()

  "SetGet" should {
    val agentSessionId = UUID.randomUUID()
    val eventKey = "content"

    "retrieve english Profile between UI and Store with a public queue" in {
      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey, Locale.ENGLISH.toString())
      Thread.sleep(3000)
      AgentHostCombinedBase.countProfile(uiR, cnxnJenJen, agentSessionId, eventKey, None) must be_==(1).eventually(5, TIMEOUT_EVENTUALLY_FOR_PA_PROCESSING)
    }

//    "retrieve french and english Profile between UI and Store with a public queue" in {
//      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey, Locale.ENGLISH.toString())
//      Thread.sleep(3000)
//      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey, Locale.FRENCH.toString())
//
//      AgentHostCombinedBase.countProfile(uiR, cnxnJenJen, agentSessionId, eventKey, Some(Locale.FRENCH.toString())) must be_==(1).eventually(10, TIMEOUT_EVENTUALLY)
//      AgentHostCombinedBase.countProfile(uiR, cnxnJenJen, agentSessionId, eventKey, None) must be_==(2).eventually(10, TIMEOUT_EVENTUALLY)
//    }
  }

}
