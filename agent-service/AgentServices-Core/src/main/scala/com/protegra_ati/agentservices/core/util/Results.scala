package com.protegra_ati.agentservices.core.util

import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress
import java.util.UUID
import com.protegra.agentservicesstore.util.MemCache

object Results
{
  @transient lazy val client = new MemcachedClient(new InetSocketAddress("localhost", 11211))
  //so it's serializable
  final val strTrue = "true"

  def getKey(): String =
  {
    "Result" + UUID.randomUUID.toString
  }

  def trigger(key: String): Unit =
  {
    MemCache.add(key, strTrue)(client)
  }

  def triggered(key: String): Boolean =
  {
    val found = MemCache.get[ String ](key)(client)
    ( strTrue.equals(found) )
  }

  def count(key: String, value: Int): Unit =
  {
    MemCache.add(key, value.toString)(client)
  }

  def voidCount(key: String): Unit =
  {
    MemCache.replace(key, "-999")(client)
  }

  def counted(key: String): Int =
  {
    val count = spinlockCount(key)
    count match {
      case null => 0
      case _ => Integer.parseInt(count)
    }
  }

  def spinlockCount(key: String): String =
  {
    var count = MemCache.get[ String ](key)(client)
    var retries = 0
    val maxRetries = 10
    while ( count == null && retries < maxRetries ) {
      Thread.sleep(1000)
      retries = retries + 1
      count = MemCache.get[ String ](key)(client)
    }
    count
  }

}

