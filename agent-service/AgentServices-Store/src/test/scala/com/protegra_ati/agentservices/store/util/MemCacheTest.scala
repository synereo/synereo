package com.protegra_ati.agentservices.store.util

import org.specs2.runner._
import org.junit.runner._
import org.specs2.mutable._

import java.util.UUID
import org.junit._
import Assert._
import net.spy.memcached.{FailureMode, ConnectionFactoryBuilder, AddrUtil, MemcachedClient}
import java.net.InetSocketAddress
import com.protegra_ati.agentservices.store.Timeouts

class MemCacheTest extends SpecificationWithJUnit
with Timeouts
{
  val client = new MemcachedClient(new ConnectionFactoryBuilder().setDaemon(true).setFailureMode(FailureMode.Retry).build(), AddrUtil.getAddresses("127.0.0.1:11211"))
  "spymemcached client set get" should {

    "find correct value" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      client.set(key, 3600, expected)
      val found = client.get(key).asInstanceOf[ String ]
      found must be_==(expected)
    }

    "not find a missing value" in {
      val key = UUID.randomUUID().toString
      val found = client.get(key)
      found must beNull[ java.lang.Object ]
    }
    "not find an expired value" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      //timeout in seconds
      client.set(key, 1, expected)
      Thread.sleep(2000)
      val found = client.get(key).asInstanceOf[ String ]
      found must beNull[ java.lang.Object ]
    }

    "find correct value within expiry" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      client.set(key, 2, expected)
      Thread.sleep(900)
      val found = client.get(key).asInstanceOf[ String ]
      found must be_==(expected)
    }
  }

  "MemCache object set get" should {
    "find correct value" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      MemCache.set(key, expected)(client);
      val found = MemCache.get[ String ](key)(client)
      found must be_==(expected)
    }

    "not find a missing value" in {
      val key = UUID.randomUUID().toString
      val found = MemCache.get[ String ](key)(client)
      found must beNull[ java.lang.Object ]
    }

    "hasValue" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      MemCache.set(key, expected)(client);
      val found = MemCache.hasValue(key)(client)
      found must be_==(true)
    }

    "hasValue is false" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      MemCache.set(key, expected)(client);
      val found = MemCache.hasValue(UUID.randomUUID().toString)(client)
      found must be_==(false)
    }

    "replace correct value" in {
      val key = UUID.randomUUID().toString
      val expected = "test"
      MemCache.set(key, expected)(client);
      val found = MemCache.get[ String ](key)(client)
      found must be_==(expected)

      val replaced = "replaced"
      MemCache.replace(key, replaced)(client);
      val foundAgain = MemCache.get[ String ](key)(client)
      foundAgain must be_==(replaced)
    }
  }

  "MemCache object list" should {
    "add value" in {
      val key = UUID.randomUUID().toString
      val value = "test"
      val expected = value :: Nil
      MemCache.addToList[ String ](key, value)(client)
      val found = MemCache.getList[ String ](key)(client)
      found must be_==(expected)
    }

    "add 100 values" in {
      val key = UUID.randomUUID().toString
      val value = "test"
      val expected = value :: Nil
      MemCache.addToList[ String ](key, value)(client)
      val found = MemCache.getList[ String ](key)(client)
      found must be_==(expected)

      var expectedLoop = expected
      for (i <- 1 to 100)
      {
        val valueLoop = "test" + i
        expectedLoop = valueLoop :: expectedLoop
        MemCache.addToList[ String ](key, valueLoop)(client)
      }

      MemCache.getList[ String ](key)(client) must be_==(expectedLoop).eventually(3, TIMEOUT_EVENTUALLY)
    }
  }

}
