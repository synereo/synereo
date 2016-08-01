package com.protegra_ati.agentservices.store.util

import java.util.UUID

import org.scalatest.FunSuite

class MultiMapTest extends FunSuite {

  test("listenerMapTest") {
    val listeners       = new MultiMap[UUID, String]
    val agentSessionId1 = UUID.randomUUID
    val agentSessionId2 = UUID.randomUUID
    listeners.add(agentSessionId1, "tag1")
    listeners.add(agentSessionId2, "tag2")
    listeners.add(agentSessionId2, "tag3")
    assert(1 == listeners.get(agentSessionId1).length)
    assert(2 == listeners.get(agentSessionId2).length)

    listeners.remove(agentSessionId1, "tag1")
    assert(0 == listeners.get(agentSessionId1).length)

    listeners.remove(agentSessionId2, "tag2")
    assert(1 == listeners.get(agentSessionId2).length)

    listeners.remove(agentSessionId2, "tag3")
    assert(0 == listeners.get(agentSessionId2).length)
  }
}
