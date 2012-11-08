package com.protegra_ati.agentservices.core.util

import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.Specification

import java.util.UUID
import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.core.Timeouts

class ResultsTest
  extends JUnit4(ResultsTestSpecs)

object ResultsTestSpecsRunner
  extends ConsoleRunner(ResultsTestSpecs)

object ResultsTestSpecs extends Specification
with Timeouts
{

  val timeoutBetween = 0

  "trigger" should {
    "return true" in {
      val key = Results.getKey()
      Results.trigger(key)
      Results.triggered(key) must be_==(true).eventually(5,TIMEOUT_EVENTUALLY)
    }
    "return false" in {
      val key = Results.getKey()
      Results.triggered(key) must be_==(false).eventually(5,TIMEOUT_EVENTUALLY)
    }

    "count 1" in {
      val key = Results.getKey()
      Results.count(key, 1)
      Results.counted(key) must be_==(1).eventually(5,TIMEOUT_EVENTUALLY)
    }
  }

}
