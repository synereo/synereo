package com.protegra.agentservicesstore

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import java.util.UUID
import org.junit._
import Assert._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.AgentTS.mTT._
import java.net.URI
import scala.util.Random

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.model.store._
import scala.util.continuations._

class PartitionedStringMGJCombinedTest
  extends JUnit4(PartitionedStringMGJCombinedTestSpecs)

object PartitionedStringMGJCombinedTestSpecsRunner
  extends ConsoleRunner(PartitionedStringMGJCombinedTestSpecs)

object PartitionedStringMGJCombinedTestSpecs extends Specification
with RabbitTestSetup
with Timeouts
{

  "PartitionedStringMGJ" should {
    var found = false;

//    val cnxnUIStore = new AgentCnxn(("UI" + UUID.randomUUID().toString).toURI, "", ("Store" + UUID.randomUUID().toString).toURI);
//    var cnxnRandom = new AgentCnxn("CombinedTestUser".toURI, "", UUID.randomUUID().toString.toURI)
  val cnxnUIStore = new AgentCnxn(("UI").toURI, "", ("Store").toURI);
  var cnxnRandom = new AgentCnxn("CombinedTest".toURI, "", "User".toURI)

    val ui_location = "localhost".toURM.withPort(RABBIT_PORT_UI_PRIVATE)
    val store_location = "localhost".toURM.withPort(RABBIT_PORT_STORE_PRIVATE)
    val public_location = "localhost".toURM.withPort(RABBIT_PORT_STORE_PUBLIC)

    "retrieve between UI and Store with a public queue" in {

      val ui_privateQ: PartitionedStringMGJ = new PartitionedStringMGJ(ui_location, List[ URM ](store_location), None)

      //removing store causes it to work, empty list of acquaintances also works
      val store_msgQ: PartitionedStringMGJ =  new PartitionedStringMGJ(store_location, List[ URM ](), None)
//      val store_msgQ: PartitionedStringMGJ =  new PartitionedStringMGJ(store_location, List[ URM ](public_location), None)
      val store_privateQ: PartitionedStringMGJ = new PartitionedStringMGJ(store_location, List[ URM ](ui_location), None)

      //removing the get causes it to work
      val keyPublic = "contentResponsePublic(_)"
      reset {
        for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) { }
      }

      val keyPrivate = "contentRequestPrivate(_)"
      reset {
        for ( e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
          //removing e!= None causing it to work
          if ( e != None ) {
              found = true;
          }
          else {
            println("listen received - none")
          }
        }
      }

      val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"
      val value = "test"
      reset { ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value)) }

      found must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

}
