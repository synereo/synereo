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

class MultiMapTest {

    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def listenerMapTest = {
      val listeners = new MultiMap[UUID, MessageEventListener]
      val agentSessionId1 = UUID.randomUUID
      val agentSessionId2 = UUID.randomUUID
      listeners.add(agentSessionId1, new MessageEventListener() {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
      listeners.add(agentSessionId2, new MessageEventListener() {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
      listeners.add(agentSessionId2, new MessageEventListener() {
          override def setContentResponseReceived(e:SetContentResponseReceivedEvent) = {
          }
      })
    assertEquals(1, listeners.get(agentSessionId1).length)
    assertEquals(2, listeners.get(agentSessionId2).length)
    }
}
