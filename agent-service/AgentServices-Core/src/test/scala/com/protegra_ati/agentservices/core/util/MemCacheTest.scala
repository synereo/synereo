package com.protegra_ati.agentservices.core.util

import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._

import java.util.UUID
import net.spy.memcached.{AddrUtil, FailureMode, ConnectionFactoryBuilder, MemcachedClient}
import java.net.InetSocketAddress
import com.protegra_ati.agentservices.core.schema.Profile
import com.protegra_ati.agentservices.store.util.MemCache
import com.protegra_ati.agentservices.core.Timeouts
import com.protegra_ati.agentservices.core.events.MessageEventAdapter

class MemCacheTest extends SpecificationWithJUnit
with Timeouts
{

  val timeoutBetween = 0
  @transient val client = new MemcachedClient(new ConnectionFactoryBuilder().setDaemon(true).setFailureMode(FailureMode.Retry).build(), AddrUtil.getAddresses("127.0.0.1:11211"))

  "spymemcached client set get" should {
    "find correct value" in {
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"
      client.set(key, 3600, profile);
      val found = client.get(key).asInstanceOf[ Profile ]
      found.firstName must be_==(profile.firstName)
    }

    "not find a missing value" in {
      val key = UUID.randomUUID().toString
      val found = client.get(key)
      found must beNull[ java.lang.Object ]
    }
  }

  "MemCache object set get" should {
    "find correct value" in {
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"

      MemCache.set(key, profile)(client)
      val found = MemCache.get[ Profile ](key)(client)
      found.firstName must be_==(profile.firstName)
    }

    "not find a missing value" in {
      val key = UUID.randomUUID().toString
      val found = MemCache.get[ Profile ](key)(client)
      found must beNull[ java.lang.Object ]
    }
  }


  "MemCache object list" should {
    "add value" in {
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"
      val expected = profile :: Nil
      MemCache.addToList[ Profile ](key, profile)(client)
      val found = MemCache.getList[ Profile ](key)(client)
      found must be_==(expected)
    }

    "add 100 values" in {
      val key = UUID.randomUUID().toString
      var profile = new Profile()
      profile.firstName = "test"
      val expected = profile :: Nil
      MemCache.addToList[ Profile ](key, profile)(client)
      val found = MemCache.getList[ Profile ](key)(client)
      found must be_==(expected)

      var expectedLoop = expected
      for ( i <- 1 to 100 ) {
        var profileLoop = new Profile()
        profileLoop.firstName = "test" + i
        expectedLoop = profileLoop :: expectedLoop
        MemCache.addToList[ Profile ](key, profileLoop)(client)
      }

      MemCache.getList[ Profile ](key)(client) must be_==(expectedLoop).eventually(3, TIMEOUT_EVENTUALLY)
    }

//    "add 100 Adapaters" in {
//      val key = UUID.randomUUID().toString
//      val adapter = new MessageEventAdapter(key)
//      val expected = adapter :: Nil
//      MemCache.addToList[ MessageEventAdapter ](key, adapter)(client)
//      val found = MemCache.getList[ MessageEventAdapter ](key)(client)
//      found.head.eventTag must be_==(expected.head.eventTag)
//
//      var expectedLoop = expected
//      for ( i <- 1 to 100 ) {
//        val adapterLoop = new MessageEventAdapter(key + i)
//        expectedLoop = adapterLoop :: expectedLoop
//        MemCache.addToList[ MessageEventAdapter ](key, adapterLoop)(client)
//      }
//
//      MemCache.getList[ MessageEventAdapter ](key)(client).head.eventTag must be_==(expectedLoop.head.eventTag).eventually(3, TIMEOUT_EVENTUALLY)
//      MemCache.getList[ MessageEventAdapter ](key)(client).last.eventTag must be_==(expectedLoop.last.eventTag).eventually(3, TIMEOUT_EVENTUALLY)
//    }
  }

}
