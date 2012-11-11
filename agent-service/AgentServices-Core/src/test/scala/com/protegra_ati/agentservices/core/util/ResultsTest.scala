package com.protegra_ati.agentservices.core.util

import org.specs2.mutable._

import java.util.UUID
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.core.Timeouts

class ResultsTest extends SpecificationWithJUnit
with Timeouts
{

  val timeoutBetween = 0

  "trigger" should {
    "saved" in {
      val key = Results.getKey()
      val profile = new Profile()
      profile.firstName = "test"
      Results.save(key, profile)
      Results.saved(key) must be_==(profile)
    }
  }

}
