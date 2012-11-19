package com.protegra.agentservicesstore

import org.specs2.specification.Scope
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import com.biosimilarity.lift.model.store._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._

import java.net.URI
import java.util.UUID

import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra.agentservicesstore.usage.AgentUseCase._

import com.biosimilarity.lift.lib.Journalist

import Being.AgentKVDBNodeFactory
import util.Results

trait KNodeSetup extends Scope
with KVDBHelpers
with RabbitTestSetup
with Timeouts
with Journalist
with FJTaskRunners
with Serializable
{  
  val cnxnUIStore = new AgentCnxn(( "UI" + UUID.randomUUID.toString ).toURI, "", ( "Store" + UUID.randomUUID.toString ).toURI)
  var cnxnRandom = new AgentCnxn(( "KUpdateTest" + UUID.randomUUID.toString ).toURI, "", ( "User" + UUID.randomUUID.toString ).toURI)

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

  var vBarrier = 0
  var kBarrier = 0

  def loop() : Unit = {
    reset {
      for (
	e <- store_privateQ.get(
	  cnxnUIStore
	)( keyPrivate.toLabel ) ) {
	  if ( e != None ) {	  
            val result = e.dispatch
	    tweet(
	      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	      + "\nlisten received - " + result
	      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	    )
	    
            Results.saveString(resultKey, result)
	    vBarrier += 1
	    spawn { loop() }
	  }
	  else {
            tweet(
	      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	      + "\nlisten received - none"
	      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	    )
	    kBarrier += 1
	  }
	}
    }
  }

  tweet(
    "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
    + "\n calling loop "
    + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
  )
  loop()
  tweet(
    "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
    + "\n loop called "
    + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
  )
}

class AgentKVDBNodeKUpdateTest extends SpecificationWithJUnit
with SpecsKVDBHelpers
with RabbitTestSetup
with Timeouts
with Journalist
with Serializable
{
  sequential

    //TODO: they all work individually but not together. need an after spec?
  "AgentKVDBNode" should {

    //    "retrieve between UI and Store with a public queue" in new KNodeSetup{
    //
    //      val value = "test@protegra.com"
    //      reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
    //
    //      Results.savedString(resultKey) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
    //    }

    //    "retrieve between UI and Store with a public queue using the persisted continuation" in new KNodeSetup{
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
    "retrieve between UI and Store with a public queue using the migrated continuation" in new KNodeSetup
    {      
      val value = "test"
      var count = 0

      while ( ( kBarrier < 1 ) && ( count < 50 ) ) {
	tweet( "waiting to get over kBarrier" )
	tweet( "count = " + count )
	count += 1
	Thread.sleep(TIMEOUT_MED)
      }
      
      count = 0

      reset {
	ui_privateQ.put(
	  cnxnUIStore
	)( keyMsg.toLabel, Ground( value ) )
      }

      while ( ( vBarrier < 1 ) && ( count < 100 ) ) {
	tweet( "waiting to get over vBarrier" )
	tweet( "count = " + count )
	count += 1
	Thread.sleep(TIMEOUT_MED)
      }

      reset {
	ui_privateQ.put(
	  cnxnUIStore
	)( keyMsg.toLabel, Ground( value ) )
      }

      //Results.savedString(restoredKey) must be_==(restored).eventually(10, TIMEOUT_EVENTUALLY)

    }
  }

}
