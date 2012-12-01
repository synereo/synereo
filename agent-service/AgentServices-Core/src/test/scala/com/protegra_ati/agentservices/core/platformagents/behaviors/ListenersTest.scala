package com.protegra_ati.agentservices.core.platformagents.behaviors

import java.util.UUID
import org.junit._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages.content._
import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._
import com.protegra_ati.agentservices.core.events._
import java.util.HashMap
import com.protegra_ati.agentservices.core._
import org.specs2.specification.Scope
import platformagents.AgentHostUIPlatformAgent
import com.protegra_ati.agentservices.core.util.Results
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}
import scala.concurrent.ops._

trait ListenerScope extends Scope
  with Serializable
{
  val mockListener = new AgentHostUIPlatformAgent
  val agentSessionId = UUID.randomUUID()
}

class ListenersTest extends SpecificationWithJUnit
with Timeouts
with Serializable
{
  "listener" should {

    "be found by message" in new ListenerScope{
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      mockListener.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(UUID.randomUUID(), "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      val list = mockListener.getListenersByMessage(mockMessage)
      list.size must be_==(1)
    }

    "be found twice by message" in new ListenerScope{
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      mockListener.addListener(agentSessionId, "1", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(agentSessionId, "2", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      Thread.sleep(TIMEOUT_MED)
      val list = mockListener.getListenersByMessage(mockMessage)
      list.size must be_==(2)
    }

    "found only once for same agentSessionId and subKey plus tag" in new ListenerScope{
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      mockListener.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      Thread.sleep(TIMEOUT_MED)
      mockListener.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(UUID.randomUUID(), "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      val list = mockListener.getListenersByMessage(mockMessage)
      list.size must be_==(1)
    }

    "found twice for same agentSessionId and subKey but different tag" in new ListenerScope{
      val mockMessage1 = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      val mockMessage2 = mockMessage1.copy(eventKey = new EventKey(agentSessionId, "fake1"))
      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage1.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage1.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage2.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.getListenersByMessage(mockMessage1).size must be_==(1)
      mockListener.getListenersByMessage(mockMessage2).size must be_==(1)
    }

    "be found by message with tag" in new ListenerScope{
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, "tag1"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      val fakeMessage = mockMessage.copy(eventKey = new EventKey(agentSessionId, "fake1"))

      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(UUID.randomUUID(), "", new MessageEventAdapter(fakeMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      val list = mockListener.getListenersByMessage(mockMessage)
      list.size must be_==(1)
    }

    "be missing by event key with tag" in new ListenerScope{
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, "tag1"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"), null)
      val fakeMessage = mockMessage.copy(eventKey = new EventKey(agentSessionId, "fake1"))

      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(UUID.randomUUID(), "", new MessageEventAdapter(fakeMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      val list = mockListener.getListenersByMessage(fakeMessage)
      list.size must be_==(0)
    }

  }

  "events" should {

    "trigger  when found" in new ListenerScope{
      val mockMessage = new SetContentResponse(new Identification(), new EventKey(agentSessionId, "testTag"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com", "CA", "someCAprovince", "city", "postalCode", "website"))

      val event = new SetContentResponseReceivedEvent(mockMessage)
      event.msg = mockMessage

      val resultKey = Results.getKey()

      mockListener.addListener(agentSessionId, "set1", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          Results.trigger(resultKey)
        }
      });

      mockListener.addListener(agentSessionId, "set2", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(agentSessionId, "get1", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.triggerEvent(event)
      Results.triggered(resultKey) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }

    "trigger synchronous" in new ListenerScope{
      val mockMessage = new SetContentResponse(new Identification(), new EventKey(agentSessionId, "testTag"),
        new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ))

      val event = new SetContentResponseReceivedEvent(mockMessage)
      event.msg = mockMessage

      val locked = new AtomicBoolean(false)
      val concurrencyError = new AtomicBoolean(false)
      val numDone = new AtomicInteger(0)
      val numThreads = 100

      mockListener.addListener(agentSessionId, "set1", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          if (locked.get())
            concurrencyError.set(true)

          locked.set(true)
          Thread.sleep(25)
          locked.set(false)

          numDone.incrementAndGet()
        }
      });

      for ( i <- 1 to numThreads) {
        spawn {
          mockListener.triggerEvent(event)
        }
      }

      numDone.get() must be_==(numThreads).eventually(5, TIMEOUT_EVENTUALLY)
      concurrencyError.get() must be_==(false)
    }


  }
}