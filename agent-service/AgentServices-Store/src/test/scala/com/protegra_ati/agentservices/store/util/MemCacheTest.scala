package com.protegra_ati.agentservices.store.util

import java.util.UUID

import net.spy.memcached.{AddrUtil, ConnectionFactoryBuilder, FailureMode, MemcachedClient}
import org.scalatest.concurrent.{Eventually, IntegrationPatience}
import org.scalatest.{MustMatchers, WordSpec}

class MemCacheTest extends WordSpec with MustMatchers with Eventually with IntegrationPatience {

  val client = new MemcachedClient(new ConnectionFactoryBuilder().setDaemon(true).setFailureMode(FailureMode.Retry).build(),
                                   AddrUtil.getAddresses("127.0.0.1:11211"))

  "spymemcached client set get" should {

    "find correct value" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      client.set(key, 3600, expected)
      val found: String = client.get(key).asInstanceOf[String]
      found must ===(expected)
    }

    "not find a missing value" in {
      val key: String   = UUID.randomUUID().toString
      val found: AnyRef = client.get(key)
      found must be(null)
    }

    "not find an expired value" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      client.set(key, 1, expected)
      def found: String = client.get(key).asInstanceOf[String]
      eventually { found must be(null) }
    }

    "find correct value within expiry" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      client.set(key, 2, expected)
      def found: String = client.get(key).asInstanceOf[String]
      eventually { found must ===(expected) }
    }
  }

  "MemCache object set get" should {

    "find correct value" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      MemCache.set(key, expected)(client)
      val found: String = MemCache.get[String](key)(client)
      found must ===(expected)
    }

    "not find a missing value" in {
      val key: String   = UUID.randomUUID().toString
      val found: String = MemCache.get[String](key)(client)
      found must be(null)
    }

    "hasValue" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      MemCache.set(key, expected)(client)
      val found: Boolean = MemCache.hasValue(key)(client)
      found must ===(true)
    }

    "hasValue is false" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      MemCache.set(key, expected)(client)
      val found: Boolean = MemCache.hasValue(UUID.randomUUID().toString)(client)
      found must ===(false)
    }

    "replace correct value" in {
      val key: String      = UUID.randomUUID().toString
      val expected: String = "test"
      MemCache.set(key, expected)(client)
      val found: String = MemCache.get[String](key)(client)
      found must ===(expected)

      val replaced: String = "replaced"
      MemCache.replace(key, replaced)(client)
      val foundAgain: String = MemCache.get[String](key)(client)
      foundAgain must ===(replaced)
    }
  }

  "MemCache object list" should {

    "add value" in {
      val key: String            = UUID.randomUUID().toString
      val value: String          = "test"
      val expected: List[String] = value :: Nil
      MemCache.addToList[String](key, value)(client)
      val found: List[String] = MemCache.getList[String](key)(client)
      found must ===(expected)
    }

    "add 100 values" in {
      val key: String            = UUID.randomUUID().toString
      val value: String          = "test"
      val expected: List[String] = value :: Nil
      MemCache.addToList[String](key, value)(client)
      val found: List[String] = MemCache.getList[String](key)(client)
      found must ===(expected)

      var expectedLoop: List[String] = expected
      for (i <- 1 to 100) {
        val valueLoop: String = "test" + i
        expectedLoop = valueLoop :: expectedLoop
        MemCache.addToList[String](key, valueLoop)(client)
      }

      eventually { MemCache.getList[String](key)(client) must ===(expectedLoop) }
    }
  }

}
