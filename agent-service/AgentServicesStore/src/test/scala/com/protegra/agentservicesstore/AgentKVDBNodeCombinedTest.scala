package com.protegra.agentservicesstore

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.biosimilarity.lift.model.store._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import Being.AgentKVDBNodeFactory

class AgentKVDBNodeCombinedTest
  extends JUnit4(AgentKVDBNodeCombinedTestSpecs)

object AgentKVDBNodeCombinedTestSpecsRunner
  extends ConsoleRunner(AgentKVDBNodeCombinedTestSpecs)

object AgentKVDBNodeCombinedTestSpecs extends Specification
with SpecsKVDBHelpers
with RabbitTestSetup
with Timeouts
{

  "AgentKVDBNode" should {
    val cnxnUIStore = new AgentCnxn(( "UI" + UUID.randomUUID.toString ).toURI, "", ( "Store" + UUID.randomUUID.toString ).toURI);
    var cnxnRandom = new AgentCnxn(( "CombinedTest" + UUID.randomUUID.toString ).toURI, "", ( "User" + UUID.randomUUID.toString ).toURI)

    val ui_location = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
    val store_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
    val public_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)

    val testId = UUID.randomUUID().toString()
    val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

//    "retrieve between UI and Store with a public queue" in {
//
//      val ui_privateQ = createNode(ui_location, List(store_location))
//
//      val store_privateQ = createNode(store_location, List(ui_location))
//      val store_msgQ = createNode(public_location, List())
//
//      val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"
//
//      val keyPublic = "contentResponsePublic(_)"
//      reset {
//        for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
//      }
//
//      val keyPrivate = "contentRequestPrivate(_)"
//      reset {
//        for ( e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
//          if ( e != None ) {
//            val result = e.dispatch
//            reset {_resultsQ.put(cnxnTest)(result.toLabel, result)}
//          }
//          else {
//            println("listen received - none")
//          }
//        }
//      }
//
//      val value = "test(1)"
//      Thread.sleep(TIMEOUT_MED)
//      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
//
//      Thread.sleep(TIMEOUT_MED)
//      fetchString(_resultsQ, cnxnTest, value.toLabel) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
//    }
//
//    "retrieve between UI and Store with a public queue using the persisted continuation" in {
//
//      val ui_privateQ = createNode(ui_location, List(store_location))
//
//      var store_privateQ = createNode(store_location, List(ui_location))
//      val store_msgQ = createNode(public_location, List())
//
//      val keyPublic = "contentResponsePublic(_)"
//      reset {
//        for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
//      }
//
//      val keyPrivate = "contentRequestPrivate(_)"
//      reset {
//        for ( e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
//          if ( e != None ) {
//            val result = e.dispatch
//            reset {_resultsQ.put(cnxnTest)(result.toLabel, result)}
//          }
//          else {
//            println("listen received - none")
//          }
//        }
//      }
//
//      Thread.sleep(TIMEOUT_MED)
//      store_privateQ = null
//      Thread.sleep(TIMEOUT_MED)
//
//      val restored_msgQ = createNode(public_location, List())
//
//      val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"
//      val value = "test(1)"
//      Thread.sleep(TIMEOUT_MED)
//      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
//
//      Thread.sleep(TIMEOUT_MED)
//      fetchString(_resultsQ, cnxnTest, value.toLabel) must be_==(value).eventually(5, TIMEOUT_EVENTUALLY)
//
//    }


    "retrieve between UI and Store with a public queue using the migrated continuation" in {

      val ui_privateQ = createNode(ui_location, List(store_location))

      var store_privateQ = createNode(store_location, List(ui_location))
      val store_msgQ = createNode(public_location, List())

      val keyPublic = "contentResponsePublic(_)"
      reset {
        for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
      }

      val keyPrivate = "contentRequestPrivate(_)"
      reset {
        for ( e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
          if ( e != None ) {
            val result = e.dispatch
            reset {_resultsQ.put(cnxnTest)(result.toLabel, result)}
          }
          else {
            println("listen received - none")
          }
        }
      }

      Thread.sleep(TIMEOUT_LONG)
      store_privateQ = null

      val restored_privateQ = createNode(public_location, List())
      val restored = "restored"
      reset {
        val generator = restored_privateQ.resubmitGet(cnxnUIStore)(keyPrivate.toLabel).getOrElse( throw new Exception( "No generator!" ) )
        for ( placeInstance <- generator ) {
          Thread.sleep(TIMEOUT_LONG)
          Thread.sleep(TIMEOUT_LONG)
          Thread.sleep(TIMEOUT_LONG)
            reset {
              Thread.sleep(TIMEOUT_LONG)
              Thread.sleep(TIMEOUT_LONG)
              Thread.sleep(TIMEOUT_LONG)
              for ( e <- restored_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
                if ( e != None ) {
                  val result = e.dispatch
                  reset {_resultsQ.put(cnxnTest)(result.toLabel, restored)}
                }
                else {
                  println("listen received - none")
                }
              }
            }
          }
      }

      //intermittent?
      val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"
      val value = "test(\"1\")"
      Thread.sleep(TIMEOUT_LONG)
      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}

      Thread.sleep(TIMEOUT_MED)
      fetchString(_resultsQ, cnxnTest, value.toLabel) must be_==(restored).eventually(5, TIMEOUT_EVENTUALLY)

    }

  }

}
