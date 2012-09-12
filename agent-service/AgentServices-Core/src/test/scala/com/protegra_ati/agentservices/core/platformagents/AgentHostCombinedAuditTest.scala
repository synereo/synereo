package com.protegra_ati.agentservices.core.platformagents

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import java.util.UUID
import org.junit._
import Assert._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import java.net.URI
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra_ati.agentservices.core.schema._
import invitation._
import scala.util.Random

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.events._
import com.biosimilarity.lift.lib.moniker._
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core._

class AgentHostCombinedAuditTest
  extends JUnit4(AgentHostCombinedAuditTestSpecs)

object AgentHostCombinedAuditTestSpecsRunner
  extends ConsoleRunner(AgentHostCombinedAuditTestSpecs)

object AgentHostCombinedAuditTestSpecs extends Specification
with Timeouts
{
  val jenUID = UUID.randomUUID()
  var cnxnJenJen = new AgentCnxn(( "Jen" + jenUID.toString ).toURI, "CombinedTestUser", ( "Jen" + jenUID.toString ).toURI)

  val mikeUID = UUID.randomUUID()
  var cnxnMikeMike = new AgentCnxn(( "Mike" + mikeUID.toString ).toURI, "CombinedTestUser", ( "Mike" + mikeUID.toString ).toURI)


  AgentHostCombinedBase.setup(this)
  val uiR = AgentHostCombinedBase.uiRef
  val storeR = AgentHostCombinedBase.storeRef


 // "Viewing a profile" ->- ( AgentHostCombinedBase.setup(this) ) should {
  "Viewing a profile" should {

    val agentSessionId = UUID.randomUUID()
    val eventKey = "content"
    AgentHostCombinedBase.setupIncrementalDisclosure(storeR, cnxnJenJen)
    storeR._cnxnUserSelfConnectionsList = List(cnxnJenJen, cnxnMikeMike)
    //  as soon as an new connection is stored, (esp. self connection) it is necessary to listen to this connection or like here to ALL selfconnections
    //  If no listening happened than connection as if would be "not visible"
    storeR.listenForHostedCnxns()

    "not return audit trail by composite search" in {

      val connJenMike = AgentHostCombinedBase.setupPersistedConnection(storeR, jenUID, mikeUID)
      storeR.updateData(cnxnJenJen, connJenMike, null)

      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey)
      //to create audit
      AgentHostCombinedBase.countProfile(uiR, cnxnJenJen, agentSessionId, eventKey) must be_==(1).eventually(10, TIMEOUT_EVENTUALLY)

      AgentHostCombinedBase.countAudit(uiR, cnxnJenJen, agentSessionId, eventKey) must be_==(0).eventually(5, TIMEOUT_EVENTUALLY)
    }

    //see if this can expose an intermittent 3 audit record write for 1 read
    "return audit trail by composite search" in {
      AgentHostCombinedBase.setupIncrementalDisclosure(storeR, cnxnJenJen)
      storeR._cnxnUserSelfConnectionsList = List(cnxnJenJen, cnxnMikeMike)

      val connJenMike = AgentHostCombinedBase.setupPersistedConnection(storeR, jenUID, mikeUID)
      storeR.updateData(cnxnJenJen, connJenMike, null)

      val connMikeJen = AgentHostCombinedBase.setupPersistedConnection(storeR, mikeUID, jenUID)

      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey)
      //to create audit
      AgentHostCombinedBase.countProfile(uiR, connMikeJen.readCnxn, agentSessionId, eventKey) must be_==(1).eventually(5, TIMEOUT_EVENTUALLY)

      // countAudit(ui, cnxnJenSelf, agentSessionId, eventKey) must be_==(1).eventually(5, TIMEOUT_EVENTUALLY)
    }

    //see if this can expose an intermittent 3 audit record write for 1 read
    "return profile by composite search" in {
      AgentHostCombinedBase.setupIncrementalDisclosure(storeR, cnxnJenJen)
      storeR._cnxnUserSelfConnectionsList = List(cnxnJenJen, cnxnMikeMike)

      val connJenMike = AgentHostCombinedBase.setupPersistedConnection(storeR, jenUID, mikeUID)
      storeR.updateData(cnxnJenJen, connJenMike, null)

      val connMikeJen = AgentHostCombinedBase.setupPersistedConnection(storeR, mikeUID, jenUID)
      storeR.updateData(cnxnMikeMike, connMikeJen, null)

      AgentHostCombinedBase.setProfile(uiR, cnxnJenJen, agentSessionId, eventKey)

      //composite
      AgentHostCombinedBase.countCompositeProfile(uiR, cnxnMikeMike, agentSessionId, eventKey) must be_==(1).eventually(5, TIMEOUT_EVENTUALLY)

    }


  }
}
