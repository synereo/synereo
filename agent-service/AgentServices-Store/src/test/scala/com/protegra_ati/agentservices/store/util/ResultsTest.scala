package com.protegra_ati.agentservices.store.util

import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

class ResultsTest extends WordSpec with MustMatchers with Eventually with IntegrationPatience {

  "trigger" should {

    "return true" in {
      val key = Results.getKey()
      Results.trigger(key)
      eventually { Results.triggered(key) must ===(true) }
    }

    "return false" in {
      val key = Results.getKey()
      eventually { Results.triggered(key) must ===(false) }
    }

    "count 1" in {
      val key = Results.getKey()
      Results.count(key, 1)
      Results.counted(key) must ===(1)
    }

    "not spinlock to 100% cpu on not found" in {
      val key: String = Results.getKey()
      // set to 0 if you want a full spinlock
      Results.counted(key, 5) must ===(0)
    }
  }
}
