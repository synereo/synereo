/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.store.util

import java.util.UUID
import org.junit._
import Assert._
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
      val listeners = new MultiCacheMap[String]("T")
      val agentSessionId1 = UUID.randomUUID
      val agentSessionId2 = UUID.randomUUID
      listeners.add(agentSessionId1.toString, "tag1")
      listeners.add(agentSessionId2.toString, "tag2")
      listeners.add(agentSessionId2.toString, "tag3")
    assertEquals(1, listeners.get(agentSessionId1.toString).toList.length)
    assertEquals(2, listeners.get(agentSessionId2.toString).toList.length)
    }
}
