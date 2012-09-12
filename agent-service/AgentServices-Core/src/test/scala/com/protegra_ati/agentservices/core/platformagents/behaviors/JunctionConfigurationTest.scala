package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import org.junit._

import org.specs._
import org.specs.util._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import net.lag.configgy._
import java.net._
import com.biosimilarity.lift.lib.moniker._

class JunctionConfigurationTest
  extends JUnit4(JunctionConfigurationTestSpecs)

object JunctionConfigurationTestSpecsRunner
  extends ConsoleRunner(JunctionConfigurationTestSpecs)

object JunctionConfigurationTestSpecs extends Specification
  with JunctionConfiguration
{
  val configFilePath = "initForUnitTest.conf"

  "loadURIs" should {
    "find 2 acquaintances from config file" in {
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val acquaintanceMapKey = "public.acquaintances"
      val acquaintanceAddresses = loadURMs(configUtil.getConfigMap(acquaintanceMapKey))

      acquaintanceAddresses.size must be_==(2)
    }

    "not fail when value not found from config file" in {
      val expected = List[URM]()
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val selfMapKey = "fake.one"
      val location = loadURMs(configUtil.getConfigMap(selfMapKey))

      location must be_==(expected)
    }
  }

  "loadFirstURI" should {
    "find 1 self from config file" in {
      val expected = "127.0.0.1".toURM.withPort(4000)
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val selfMapKey = "public.self"
      val location = loadFirstURM(configUtil.getConfigMap(selfMapKey))

      location must be_==(expected)
    }
  }

}