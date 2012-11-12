/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.platformagents


import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.events._
import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._

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
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.mTT._
import com.protegra_ati.agentservices.core.messages._
import moniker._
import scala.util.continuations._
import scala.util.Random
import java.net.{URI}
import verifier._
import com.protegra_ati.agentservices.core._
import com.protegra_ati.agentservices.core.util.Results


class AgentHostUIPlatformAgentTest extends SpecificationWithJUnit
with Timeouts
with Serializable
{
  val rand = new Random()

  val sourceId = UUID.randomUUID()
  val targetId = sourceId
  val cnxn = new AgentCnxnProxy(sourceId.toString.toURI, "", targetId.toString.toURI)
  val cnxnUIStore = new AgentCnxnProxy(( "UI" + UUID.randomUUID().toString ).toURI, "", ( "Store" + UUID.randomUUID().toString ).toURI);
  val pa = createPA

  def createPA: AgentHostUIPlatformAgent =
  {
    val sourceAddress = "127.0.0.1".toURI
    val acquaintanceAddresses = List[ URI ]()
    val pa = new AgentHostUIPlatformAgent()
    pa._cnxnUIStore = cnxnUIStore
    pa.initForTest(sourceAddress, acquaintanceAddresses, UUID.randomUUID)
    pa
  }

  "receiving a response message" should {
    "raise a SetContentResponseReceivedEvent" in {
      val agentSessionId = UUID.randomUUID
      val key = Results.getKey()

      val parentIds = new Identification()
      val req = new SetContentResponse(parentIds.copyAsChild(), new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None

      pa.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          Results.trigger(key)
        }
      });

      pa.send(req)
      Thread.sleep(TIMEOUT_LONG)
      Results.triggered(key) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }

    "raise a GetContentResponseReceivedEvent" in {
      val agentSessionId = UUID.randomUUID
      val key = Results.getKey()

      val parentIds = new Identification()
      val req = new GetContentResponse(parentIds.copyAsChild(), new EventKey(agentSessionId, ""), List(new Profile("testFirst", "testLast", "test Description", "123@test.com", "CA", "someCAprovince", "city", "postalCode", "website")))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None

      pa.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
          Results.trigger(key)
        }
      });

      pa.send(req)
      //Thread.sleep(TIMEOUT_LONG)
      Results.triggered(key) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

  "listeners" should {
    "trigger the right event" in {

      val userAgentId1 = UUID.randomUUID
      val userAgentId2 = UUID.randomUUID
      val key = Results.getKey()

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
          Results.trigger(key)
        }
      })

      val parentIds = new Identification()
      val req = SetContentResponse(parentIds.copyAsChild(), new EventKey(userAgentId2, ""), new Profile("testFirst", "testLast", "test Description", "1@test.com", "CA", "someCAprovince", "city", "postalCode", "website"))
      //Responses now default to a public channel level so for this test need to turn that off
      req.channelLevel = None

      pa.send(req)
      Results.triggered(key) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }

}