/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.platformagents


import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.events._
import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra.agentservicesstore.extensions.OptionExtensions._
import actors.threadpool.LinkedBlockingQueue
import net.lag._
import java.util.UUID
import com.protegra_ati.agentservices.core.schema._
import org.junit._
import Assert._
import com.biosimilarity.lift.lib._
import com.protegra_ati.agentservices.core.messages.content._
import com.protegra.agentservicesstore.AgentTS._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.AgentTS.mTT._
import com.protegra_ati.agentservices.core.messages._
import moniker._
import scala.util.continuations._
import scala.util.Random
import java.net.URI
import org.specs.runner._
import verifier._
import com.protegra_ati.agentservices.core._


class AgentHostUIPlatformAgentTest
  extends JUnit4(AgentHostUIPlatformAgentTestSpecs)

object AgentHostUIPlatformAgentTestSpecsRunner
  extends ConsoleRunner(AgentHostUIPlatformAgentTestSpecs)

object AgentHostUIPlatformAgentTestSpecs extends Specification
  with Timeouts
{

  val rand = new Random()

  val sourceId = UUID.randomUUID()
  val targetId = sourceId
  val cnxn = new AgentCnxn(sourceId.toString.toURI, "", targetId.toString.toURI)

  def createPA: AgentHostUIPlatformAgent =
  {
    val sourceAddress = "127.0.0.1".toURM
    val acquaintanceAddresses = List[URM]()
    val pa = new AgentHostUIPlatformAgent()
//    pa._cnxnUIStore = cnxn
    pa.initForTest(sourceAddress, acquaintanceAddresses, UUID.randomUUID)
    pa
  }

  //really a kvdb test
  "privateQ" should {
    val pa = createPA
    "add a partition" in {
      val startSize = pa._privateQ.cnxnPartition.size
      val cnxn = new AgentCnxn("test".toURI, "", UUID.randomUUID().toString.toURI)
      pa._privateQ.getPartition(cnxn)
      pa._privateQ.cnxnPartition.size must be_==(startSize+1).eventually(20, TIMEOUT_EVENTUALLY)
    }
  }

  "receiving a response message" should {
    val pa = createPA
    var triggered = false
    "raise a SetContentResponseReceivedEvent" in {
      val agentSessionId = UUID.randomUUID
      val parentIds = new Identification()
      val req = new SetContentResponse(parentIds.copyAsChild(), new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None
      pa.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          triggered = true
        }
      });

      pa.send(req)
      //Thread.sleep(TIMEOUT_LONG)
      pa.listenPrivate(pa._cnxnUIStore)
      triggered must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }

    "raise a GetContentResponseReceivedEvent" in {
      val parentIds = new Identification()
      val agentSessionId = UUID.randomUUID
      val req = new GetContentResponse(parentIds.copyAsChild(), new EventKey(agentSessionId, ""), List(new Profile("testFirst", "testLast", "test Description", "123@test.com","CA", "someCAprovince", "city", "postalCode", "website" )))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None

      pa.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
          triggered = true
        }
      });

      pa.send(req)
      //Thread.sleep(TIMEOUT_LONG)
      pa.listenPrivate(pa._cnxnUIStore)
      triggered must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }
  }

  "listeners" should {
    val pa = createPA
    var triggered = false
    "trigger the right event" in {

      val userAgentId1 = UUID.randomUUID
      val userAgentId2 = UUID.randomUUID

      pa.addListener(userAgentId1, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          fail()
        }
      })

      pa.addListener(userAgentId2, "get", new MessageEventAdapter()
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
          fail()
        }
      })

      pa.addListener(userAgentId2, "set", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          triggered = true
        }
      })

      val parentIds = new Identification()
      val req = SetContentResponse(parentIds.copyAsChild(), new EventKey(userAgentId2, ""), new Profile("testFirst", "testLast", "test Description", "1@test.com","CA", "someCAprovince", "city", "postalCode", "website" ))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None

      pa.send(req)
      triggered must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }

    "raise a GetContentAuthorizationRequiredNotification" in {

      val userAgentId1 = UUID.randomUUID

      pa.addListener(userAgentId1, "", new MessageEventAdapter()
      {
        override def getContentAuthorizationRequiredNotificationReceived(e: GetContentAuthorizationRequiredNotificationReceivedEvent) =
        {
          triggered = true
        }
      })

      //why no parent ids on notification    //parentIds.copyAsChild(),
//      val parentIds = new Identification()
      val req = GetContentAuthorizationRequiredNotification(new EventKey(userAgentId1, ""))

      pa.send(req)
      triggered must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }
    "raise a VerifyContentRequestNotification" in {

      val userAgentId1 = UUID.randomUUID

      pa.addListener(userAgentId1, "", new MessageEventAdapter()
      {
        override def verifyContentRequestNotificationReceived(e: VerifyContentRequestNotificationReceivedEvent) =
        {
          triggered = true
        }
      })

      //why no parent ids on notification    //parentIds.copyAsChild(),
//      val parentIds = new Identification()
      val req = VerifyContentRequestNotification(new EventKey(userAgentId1, ""))

      pa.send(req)
      triggered must be_==(true).eventually(20, TIMEOUT_EVENTUALLY)
    }
  }

}