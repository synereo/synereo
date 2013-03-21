package com.protegra_ati.agentservices.store

import org.specs2.specification.Scope
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._

import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._


import util.Results

trait NodeSetup extends Scope
with KVDBHelpers
with RabbitTestSetup
with Timeouts
with Serializable
{
  val cnxnUIStore = new AgentCnxn(( "UI" + UUID.randomUUID.toString ).toURI, "", ( "Store" + UUID.randomUUID.toString ).toURI)
  var cnxnRandom = new AgentCnxn(( "CombinedTest" + UUID.randomUUID.toString ).toURI, "", ( "User" + UUID.randomUUID.toString ).toURI)

  val ui_location = "localhost".toURI.withPort(RABBIT_PORT_UI_PRIVATE)
  val store_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PRIVATE)
  val public_location = "localhost".toURI.withPort(RABBIT_PORT_STORE_PUBLIC)

  val testId = UUID.randomUUID().toString()
  val cnxnTest = new AgentCnxn(( "TestDB" + testId ).toURI, "", ( "TestDB" + testId ).toURI)

  val uiConfigFileName = Some("db_ui.conf")
  val storeConfigFileName = Some("db_store.conf")
  val msgConfigFileName = Some("db_store.conf")

  val ui_privateQ = createNode(ui_location, List(store_location), uiConfigFileName)

  var store_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)

  //  val store_msgQ = createNode(public_location, List(), msgConfigFileName)

  val keyMsg = "contentRequestPrivate(\"" + UUID.randomUUID() + "\")"
  //
  //  val keyPublic = "contentResponsePublic(_)"
  //  reset {
  //    for ( e <- store_msgQ.get(cnxnUIStore)(keyPublic.toLabel) ) {}
  //  }

  val resultKey = Results.getKey()

  val keyPrivate = "contentRequestPrivate(_)"
  reset {
    for ( e <- store_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
      if ( e != None ) {
        val result = e.dispatch
        Results.saveString(resultKey, result)
      }
      else {
        println("listen received - none")
      }
    }
  }
}

class AgentKVDBNodeCombinedTest extends SpecificationWithJUnit
with SpecsKVDBHelpers
with RabbitTestSetup
with Timeouts
with Serializable
{
  sequential

    //TODO: they all work individually but not together. need an after spec?
  "AgentKVDBNode" should {

    //    "retrieve between UI and Store with a public queue" in new NodeSetup{
    //
    //      val value = "test@protegra.com"
    //      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
    //
    //      Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
    //    }

    //    "retrieve between UI and Store with a public queue using the persisted continuation" in new NodeSetup{
    //
    //      Thread.sleep(TIMEOUT_MED)
    //      store_privateQ = null
    //      Thread.sleep(TIMEOUT_MED)
    //
    //      val restored_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)
    //      Thread.sleep(TIMEOUT_LONG)
    //
    //      val value = "test"
    //      Thread.sleep(TIMEOUT_MED)
    //      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
    //
    //      Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
    //
    //    }
    //RACE: see why it fails
    "retrieve between UI and Store with a public queue using the migrated continuation" in new NodeSetup
    {

      Thread.sleep(TIMEOUT_MED)
      store_privateQ = null
      Thread.sleep(TIMEOUT_MED)

      val restored_privateQ = createNode(store_location, List(ui_location), storeConfigFileName)
      Thread.sleep(TIMEOUT_LONG)

      val restoredKey = Results.getKey()
      val restored = "restored"
      reset {
        val generator = restored_privateQ.resubmitGet(cnxnUIStore)(keyPrivate.toLabel).getOrElse(throw new Exception("No generator!"))
        for ( placeInstance <- generator ) {
          reset {
            for ( e <- restored_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
              if ( e != None ) {
                val result = e.dispatch
                Results.saveString(restoredKey, restored)
              }
              else {
                println("listen received - none")
              }
            }
          }
        }
      }
      val value = "test"
      Thread.sleep(TIMEOUT_MED)
      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}

      Results.savedString(restoredKey) must be_==(restored).eventually(10, TIMEOUT_EVENTUALLY)

    }
  }

}
