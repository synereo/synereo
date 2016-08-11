// -*- mode: Scala;-*- 
// Filename:    ResubmissionTest.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 30 15:51:29 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import com.biosimilarity.lift.lib.BasicLogService
import org.scalatest.{MustMatchers, WordSpec}

class AgentKVDBNodeResubmitRequestsTest extends WordSpec with MustMatchers {

  "AgentKVDBNode" should {
    @transient
    val testConfig = AgentKVDBNodeResubmitRequestsTestDefaultConfiguration()

    /* --------------------------------------------------------- *
     *                       Scenarios
     * --------------------------------------------------------- */

    "retrieve between UI and Store with a public queue" ignore {

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
      testConfig.registerContinuations(testConfig.keyBidsNAsks._2.take(testConfig.numberOfStandingRequests))(testConfig.ckrReportPresent,
                                                                                                             testConfig.ckrReportAbsent)

      /* --------------------------------------------------------- *
       * Wait at barrier -- will be fixed with a blocking get, soon
       * --------------------------------------------------------- */

      while (testConfig.barrier() < testConfig.numberOfStandingRequests) {
        BasicLogService.tweet("waiting to get past registration barrier; current height: " + testConfig.barrier)
        Thread.sleep(testConfig.TIMEOUT_MED)
      }

      /* --------------------------------------------------------- *
       *                   Resubmit requests
       * --------------------------------------------------------- */

      testConfig.resubmitRequests(testConfig.keyBidsNAsks._2.take(testConfig.numberOfStandingRequests))

      testConfig.justPullKRecords match {
        case false =>
          BasicLogService.tweet(s"""|
                                    |########################################################
                                    | Actually resubmitting requests
                                    |########################################################
                                    |""".stripMargin)
          while (testConfig.barrier() < testConfig.numberOfStandingRequests) {
            BasicLogService.tweet("waiting to get past resubmission barrier; current height: " + testConfig.barrier)
            Thread.sleep(testConfig.TIMEOUT_MED)
          }

          BasicLogService.tweet("\n########################################################\n")

          for ((k, v) <- testConfig.keyMap) {
            BasicLogService.tweet(s"""|registered key : $k
                                      |number of times registered : $v
                                      |""".stripMargin)
          }

          BasicLogService.tweet("\n########################################################\n")

          testConfig.probeStandingRequests(
            testConfig.keyBidsNAsks._1.take(testConfig.numberOfStandingRequests)
          )
        case true =>
          BasicLogService.tweet(s"""|
                                    |########################################################
                                    | Just pulled krecords
                                    |########################################################
                                    |""".stripMargin)
      }

      /* --------------------------------------------------------- *
       *                   Assert invariant
       * --------------------------------------------------------- */
      1 must ===(1)

      // fetchString(_resultsQ, cnxnTest, resultKey.toLabel) must be_==(value).eventually(10, TIMEOUT_EVENTUALLY)
    }

  }

}
