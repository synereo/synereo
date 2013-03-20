// -*- mode: Scala;-*- 
// Filename:    ResubmissionTest.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 30 15:51:29 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import org.specs2.mutable._

import org.specs2.runner._
import org.junit.runner._
import org.specs2.matcher._

import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._

import scala.util.continuations._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import java.net.URI
import java.util.UUID

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.mTT._
import com.protegra_ati.agentservices.store.mongo.usage._



class AgentKVDBNodeResubmitRequestsTest extends SpecificationWithJUnit
{  

  "AgentKVDBNode" should {
    @transient
    val testConfig =
      AgentKVDBNodeResubmitRequestsTestDefaultConfiguration()
    
    /* --------------------------------------------------------- *
     *                       Scenarios
     * --------------------------------------------------------- */

    "retrieve between UI and Store with a public queue" in {
      skipped("no assert")

      /* --------------------------------------------------------- *
       *                       KVDBs
       * --------------------------------------------------------- */

      // val msgConfigFileName = Some("db_store.conf")
      // val store_msgQ = createNode(public_location, List(), msgConfigFileName)

      // spawn {
      //   reset {ui_privateQ.put(cnxnUIStore)(keyMsg.toLabel, Ground(value))}
      // }

      /* --------------------------------------------------------- *
       *                   Store continuations
       * --------------------------------------------------------- */      
      testConfig.registerContinuations(
	testConfig.keyBidsNAsks._2.take( testConfig.numberOfStandingRequests )
      )( testConfig.ckrReportPresent, testConfig.ckrReportAbsent )

      /* --------------------------------------------------------- *
       * Wait at barrier -- will be fixed with a blocking get, soon
       * --------------------------------------------------------- */

      while ( testConfig.barrier() < testConfig.numberOfStandingRequests ) {
	println( "waiting to get past registration barrier; current height: " + testConfig.barrier )
	Thread.sleep( testConfig.TIMEOUT_MED )
      }

      /* --------------------------------------------------------- *
       *                   Resubmit requests
       * --------------------------------------------------------- */      

      testConfig.resubmitRequests(
	testConfig.keyBidsNAsks._2.take( testConfig.numberOfStandingRequests )
      )

      testConfig.justPullKRecords match {
	case false => {
	  println( "\n########################################################\n" )
	  println( " Actually resubmitting requests " )
	  println( "\n########################################################\n" )

	  while ( testConfig.barrier() < testConfig.numberOfStandingRequests ) {
	    println( "waiting to get past resubmission barrier; current height: " + testConfig.barrier )
	    Thread.sleep( testConfig.TIMEOUT_MED )
	  }

	  println( "\n########################################################\n" )
	  
	  for( ( k, v ) <- testConfig.keyMap ) {
	    println( 	  
	      "registered key : " + k + "\n"
	      + "number of times registered : " + v + "\n"
	    )
	  }
	  
	  println( "\n########################################################\n" )
	  
	  testConfig.probeStandingRequests(
	    testConfig.keyBidsNAsks._1.take( testConfig.numberOfStandingRequests )
	  )
	}
	case true => {
	  println( "\n########################################################\n" )
	  println( " Just pulled krecords " )
	  println( "\n########################################################\n" )
	}
      }      

      /* --------------------------------------------------------- *
       *                   Assert invariant
       * --------------------------------------------------------- */      
      1 must be_==(1)

      //fetchString(_resultsQ, cnxnTest, resultKey.toLabel) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
    }

  }

}
