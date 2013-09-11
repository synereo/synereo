package com.protegra_ati.agentservices.store

import org.specs2.specification.Scope
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._

import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import scala.util.continuations._
import scala.collection.mutable.HashMap

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._

import util.Results

trait PubSubKNodeSetup extends Scope
with KVDBHelpers
with RabbitTestSetup
with Timeouts
with ThreadPoolRunnersX
//with FJTaskRunnersX
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

  object OutOfMemoryCounter {
    import com.biosimilarity.lift.lib._

    lazy val srcScope = new AMQPStdScope[String]()
    lazy val srcQM =
      new srcScope.AMQPQueueHostExchangeM[String]( "localhost", "TestObservations" )
    lazy val srcQ = srcQM.zero[String]

    def recordObservation( observation : String ) : String = {
      report(
	"----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	+ "\nrecording observation: " + observation
	+ "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
      )
      srcQ ! observation
      observation
    }
    def reportObservations( ) : Unit = {
      report(
	"----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	+ "\nreporting observations"
	+ "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
      )
      val observationMap : HashMap[String,Int] =
	new HashMap[String,Int]()

      for( msg <- srcQM( srcQ ) ) {
	report(
	  "observed " + msg + " " + ( observationMap.get( msg ).getOrElse( 0 ) + 1 ) + " times"
	)
	observationMap += ( msg -> ( observationMap.get( msg ).getOrElse( 0 ) + 1 ) )
      }
      for( ( k, v ) <- observationMap ) {
	report(
	  "observed " + k + " " + v + " times"
	)
      }
      report(
	"----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	+ "\nobservations report"
	+ "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
      )
    }
  }

  def subscription() : Unit = {
    reset {
      for (
	e <- store_privateQ.subscribe(
	  cnxnUIStore
	)( keyPrivate.toLabel ) ) {
	  if ( e != None ) {	  
            val result = e.dispatch
	    report(
	      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	      + "\nsubscribe received - " + result
	      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	    )	    
	    
	    OutOfMemoryCounter.recordObservation( result )

            Results.saveString( resultKey, result )
	    vBarrier += 1
	    //spawn { subscription() }
	  }
	  else {
            report(
	      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	      + "\nsubscribe received - none"
	      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	    )
	    kBarrier += 1
	  }
	}
    }
  }

  def publication( n : Int, keyMsg : String, value : String ) : Unit = {
    n match {
      case i : Int if i > 0 => {
	val pval : String = value + UUID.randomUUID.toString
	report(
	  "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	  + "\npublishing " + pval + " to " + keyMsg
	  + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	)
	reset {
	  ui_privateQ.publish(
	    cnxnUIStore
	  )( keyMsg.toLabel, Ground( pval ) )
	}
	publication( n - 1, keyMsg, value )
      }
      case _ => {
	report(
	  "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	  + "\npublication complete"
	  + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
	)
      }
    }    
  }

  def testBehavior(
    value : String, barrierCount : Int, useBarrier : Boolean
  ) : Unit = {
    var count = 0      
   
    report(
      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
      + "\n calling subscription "
      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
    )
    subscription()
    report(
      "----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
      + "\n subscription called "
      + "\n----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>----->>>>>"
    )
    
    //publication( 5, keyMsg, value )
    
    if ( useBarrier ) {
      while ( ( kBarrier < 1 ) && ( count < barrierCount ) ) {
	report( "waiting to get over kBarrier" )
	report( "count = " + count )
	count += 1
	Thread.sleep(TIMEOUT_MED)
      }
    }
    
    //publication( 5, keyMsg, value )
    
    count = 0      
    
    if ( useBarrier ) {
      while ( ( vBarrier < 1 ) && ( count < barrierCount ) ) {
	report( "waiting to get over vBarrier" )
	report( "count = " + count )
	count += 1
	Thread.sleep(TIMEOUT_MED)
      }
    }
    
    publication( 5, keyMsg, value )
    publication( 5, keyMsg, value )
    publication( 5, keyMsg, value )

    count = 0      
    
    if ( useBarrier ) {
      while ( ( vBarrier < 1 ) && ( count < barrierCount ) ) {
	report( "waiting to get over vBarrier" )
	report( "count = " + count )
	count += 1
	Thread.sleep(TIMEOUT_MED)
      }
    }
    
    OutOfMemoryCounter.reportObservations()
  }  
}

class AgentKVDBNodeKUpdatePubSubTest extends SpecificationWithJUnit
with SpecsKVDBHelpers
with RabbitTestSetup
with Timeouts
with Serializable
{
  sequential
  "AgentKVDBNode" should {

    "retrieve between UI and Store with a public queue using the migrated continuation" in new PubSubKNodeSetup
    {      
      testBehavior( "test", 50, true )
      Thread.sleep(TIMEOUT_MED)
      //Results.savedString(restoredKey) must be_==(restored).eventually(10, TIMEOUT_EVENTUALLY)
    }
  }

}
