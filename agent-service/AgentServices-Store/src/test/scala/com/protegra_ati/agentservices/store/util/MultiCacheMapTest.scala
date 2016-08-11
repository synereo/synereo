package com.protegra_ati.agentservices.store.util

import java.util.UUID

import org.scalatest.FunSuite

import scala.collection.JavaConversions._

class MultiCacheMapTest extends FunSuite {

  test("Listener Map Test") {
    val listeners: MultiCacheMap[String] = new MultiCacheMap[String]("T")
    val agentSessionId1: UUID            = UUID.randomUUID
    val agentSessionId2: UUID            = UUID.randomUUID
    listeners.add(agentSessionId1.toString, "tag1")
    listeners.add(agentSessionId2.toString, "tag2")
    listeners.add(agentSessionId2.toString, "tag3")
    assert(1 == listeners.get(agentSessionId1.toString).toList.length)
    assert(2 == listeners.get(agentSessionId2.toString).toList.length)
  }
}
