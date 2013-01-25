package com.protegra_ati.agentservices.store.util

import org.specs2.mutable._
import org.specs2.time.Duration

import java.util.UUID
import com.protegra_ati.agentservices.store.Timeouts


class ResultsTest extends SpecificationWithJUnit
with Timeouts
{

  "trigger" should {
    "return true" in {
      val key = Results.getKey()
      Results.trigger(key)
      Results.triggered(key) must be_==(true).eventually(5, TIMEOUT_EVENTUALLY)
    }
    "return false" in {
      val key = Results.getKey()
      Results.triggered(key) must be_==(false).eventually(5, TIMEOUT_EVENTUALLY)
    }

    "count 1" in {
      val key = Results.getKey()
      Results.count(key, 1)
      Results.counted(key) must be_==(1)
    }

    "not spinlock to 100% cpu on not found" in {
      val key = Results.getKey()
           //set to 0 if you want a full spinlock
      Results.counted(key, 5) must be_==(0)
    }
  }


}
