/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.store.util

import java.util.UUID
import org.junit._
import Assert._

class MultiMapTest {

    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def listenerMapTest = {
      val listeners = new MultiMap[UUID, String]
      val agentSessionId1 = UUID.randomUUID
      val agentSessionId2 = UUID.randomUUID
      listeners.add(agentSessionId1, "tag1")
      listeners.add(agentSessionId2, "tag2")
      listeners.add(agentSessionId2, "tag3")
      assertEquals(1, listeners.get(agentSessionId1).length)
      assertEquals(2, listeners.get(agentSessionId2).length)

      listeners.remove(agentSessionId1, "tag1")
      assertEquals(0, listeners.get(agentSessionId1).length)

      listeners.remove(agentSessionId2, "tag2")
      assertEquals(1, listeners.get(agentSessionId2).length)

      listeners.remove(agentSessionId2, "tag3")
      assertEquals(0, listeners.get(agentSessionId2).length)
    }
}
