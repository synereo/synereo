package com.protegra_ati.agentservices.core.platformagents.behaviors

import java.util.UUID
import org.junit._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages.content._
import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import com.protegra_ati.agentservices.core.events._
import java.util.HashMap
import com.protegra_ati.agentservices.core._

class ListenersTest
  extends JUnit4(ListenersTestSpecs)

object ListenersTestSpecsRunner
  extends ConsoleRunner(ListenersTestSpecs)

object ListenersTestSpecs extends Specification
with Timeouts
{
  "listener" should {
    val mockListener = new Object with Listeners
    val agentSessionId = UUID.randomUUID()

    "be found by message" in {
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
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

    "be found twice by message" in {
        val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
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

        val list = mockListener.getListenersByMessage(mockMessage)
        list.size must be_==(2)
      }

    "found only once for same agentSessionId and subKey plus tag" in {
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
      mockListener.addListener(agentSessionId, "", new MessageEventAdapter()
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
        }
      });

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

    "found twice for same agentSessionId and subKey but different tag" in {
        val mockMessage1 = new SetContentRequest(new EventKey(agentSessionId, ""), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
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

    "be found by message with tag" in {
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, "tag1"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
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

    "be missing by event key with tag" in {
      val mockMessage = new SetContentRequest(new EventKey(agentSessionId, "tag1"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ), null)
      val fakeMessage = mockMessage.copy(eventKey = new EventKey(agentSessionId, "fake1"))

      mockListener.addListener(agentSessionId, "", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def getContentResponseReceived(e: GetContentResponseReceivedEvent) =
        {
        }
      });

      mockListener.addListener(UUID.randomUUID(), "",  new MessageEventAdapter(fakeMessage.eventKey.eventTag)
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

    val mockListener = new Object with Listeners
    val agentSessionId = UUID.randomUUID()

    "trigger  when found" in {
      val mockMessage = new SetContentResponse(new Identification(), new EventKey(agentSessionId, "testTag"), new Profile("testFirst", "testLast", "test Description", "bc123@test.com","CA", "someCAprovince", "city", "postalCode", "website" ))

      val event = new SetContentResponseReceivedEvent(mockMessage)
      event.msg = mockMessage

      var triggered = false

      mockListener.addListener(agentSessionId, "set1", new MessageEventAdapter(mockMessage.eventKey.eventTag)
      {
        override def setContentResponseReceived(e: SetContentResponseReceivedEvent) =
        {
          triggered = true
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

      triggered must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
  }
}