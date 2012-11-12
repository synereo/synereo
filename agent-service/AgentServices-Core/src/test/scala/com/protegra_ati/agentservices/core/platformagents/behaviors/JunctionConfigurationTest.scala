package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._
import org.junit._

import org.specs2.mutable._
import org.specs2.time.Duration
import org.junit.runner._
import org.specs2.runner._
import net.lag.configgy._
import java.net._
import java.net.URI

class JunctionConfigurationTest extends SpecificationWithJUnit
  with JunctionConfiguration
{
  val configFilePath = "initForUnitTest.conf"

  "loadURIs" should {
    "find 2 acquaintances from config file" in {
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val acquaintanceMapKey = "public.acquaintances"
      val acquaintanceAddresses = loadURIs(configUtil.getConfigMap(acquaintanceMapKey))

      acquaintanceAddresses.size must be_==(2)
    }

    "not fail when value not found from config file" in {
      val expected = List[URI]()
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val selfMapKey = "fake.one"
      val location = loadURIs(configUtil.getConfigMap(selfMapKey))

      location must be_==(expected)
    }
  }

  "loadFirstURI" should {
    "find 1 self from config file" in {
      val expected = "127.0.0.1".toURI.withPort(4000)
      Configgy.configure(configFilePath)
      val configUtil = Configgy.config

      val selfMapKey = "public.self"
      val location = loadFirstURI(configUtil.getConfigMap(selfMapKey))

      location must be_==(expected)
    }
  }

}