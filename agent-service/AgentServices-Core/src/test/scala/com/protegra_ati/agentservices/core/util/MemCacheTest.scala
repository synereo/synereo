package com.protegra_ati.agentservices.core.util

import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.Specification

import java.util.UUID
import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra.agentservicesstore.util.MemCache

class MemCacheTest
  extends JUnit4(MemCacheTestSpecs)

object MemCacheTestSpecsRunner
  extends ConsoleRunner(MemCacheTestSpecs)

object MemCacheTestSpecs extends Specification
{

  val timeoutBetween = 0

  "spymemcached client set get" should {
    "find correct value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"
      client.set(key, 3600, profile);
      val found = client.get(key).asInstanceOf[ Profile ]
      found.firstName must be_==(profile.firstName)
    }

    "not find a missing value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      val found = client.get(key)
      found must beNull[ java.lang.Object ]
    }
  }

  "MemCache object set get" should {
    "find correct value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"

      MemCache.add(key, profile)(client)
      val found = MemCache.get[ Profile ](key)(client)
      found.firstName must be_==(profile.firstName)
    }

    "not find a missing value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      val found = MemCache.get[ Profile ](key)(client)
      found must beNull[ java.lang.Object ]
    }
  }

}
