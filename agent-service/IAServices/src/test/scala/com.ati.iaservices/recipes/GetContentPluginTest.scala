package com.ati.iaservices.recipes

import com.ati.iaservices.recipes.LauncherPluginSession.session
import com.protegra_ati.agentservices.core.schema._
import org.specs2.mutable._
import java.util.UUID
import com.protegra_ati.agentservices.core.util.Results
import org.specs2.time.Duration

class GetContentPluginTest extends SpecificationWithJUnit
with Serializable
{
  "GetContentPlugin with Profile" should {
    "return a Profile" in {
      // val resultKey = Results.getKey()

      // Results.triggered(resultKey) must be_==(true).eventually(10, new Duration(2000))
    }
  }
}
