package com.protegra.agentservicesstore.util

import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.Specification

import java.util.UUID
import org.junit._
import Assert._
import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress

class MemCacheTest
  extends JUnit4(MemCacheTestSpecs)

object MemCacheTestSpecsRunner
  extends ConsoleRunner(MemCacheTestSpecs)

object MemCacheTestSpecs extends Specification
{
  "spymemcached client set get" should {
    "find correct value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      val expected = "test"
      client.set(key, 3600, expected);
      val found = client.get(key).asInstanceOf[ String ]
      found must be_==(expected)
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
      val expected = "test"
      MemCache.add(key, expected)(client);
      val found = MemCache.get[ String ](key)(client)
      found must be_==(expected)
    }

    "not find a missing value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      val found = MemCache.get[ String ](key)(client)
      found must beNull[ java.lang.Object ]
    }

    "replace correct value" in {
      val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
      val key = UUID.randomUUID().toString
      val expected = "test"
      MemCache.add(key, expected)(client);
      val found = MemCache.get[ String ](key)(client)
      found must be_==(expected)

      val replaced = "replaced"
      MemCache.replace(key, replaced)(client);
      val foundAgain = MemCache.get[ String ](key)(client)
      foundAgain must be_==(replaced)
    }
  }

}
