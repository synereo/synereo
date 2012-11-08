/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.util

import com.protegra_ati.agentservices.core.events._
import java.util.UUID
import org.junit._
import Assert._
import com.protegra_ati.agentservices.core.events.MessageEventListener
import scala.collection.JavaConversions._

class MultiCacheMapTest {

    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def listenerMapTest = {
      val listeners = new MultiCacheMap[MessageEventAdapter]("T")
      val agentSessionId1 = UUID.randomUUID
      val agentSessionId2 = UUID.randomUUID
      listeners.add(agentSessionId1.toString, new MessageEventAdapter("tag1") {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
      listeners.add(agentSessionId2.toString, new MessageEventAdapter("tag2") {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
      listeners.add(agentSessionId2.toString, new MessageEventAdapter("tag3") {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
    assertEquals(1, listeners.get(agentSessionId1.toString).toList.length)
    assertEquals(2, listeners.get(agentSessionId2.toString).toList.length)
    }
}
