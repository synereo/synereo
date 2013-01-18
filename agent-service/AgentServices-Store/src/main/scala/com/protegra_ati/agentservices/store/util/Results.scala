package com.protegra_ati.agentservices.store.util

import net.spy.memcached.{AddrUtil, FailureMode, ConnectionFactoryBuilder, MemcachedClient}
import java.util.UUID

object Results extends ResultsBase
{
}

trait ResultsBase
{
  @transient val client = new MemcachedClient(new ConnectionFactoryBuilder().setDaemon(true).setFailureMode(FailureMode.Retry).build(), AddrUtil.getAddresses("127.0.0.1:11211"))
  //so it's serializable
  final val RESULT_PREFIX = "R"
  final val strTrue = "true"


  def getKey(): String =
  {
    RESULT_PREFIX + UUID.randomUUID.toString
  }

  def trigger(key: String): Unit =
  {
    MemCache.set(key, strTrue)(client)
  }

  def triggered(key: String): Boolean =
  {
    val found = MemCache.get[ String ](key)(client)
    ( strTrue.equals(found) )
  }

  def saveString(key: String, value: String): Unit =
  {
    MemCache.set(key, value)(client)
  }

  def savedString(key: String): String =
  {
    savedString(key, 5)
  }

  def savedString(key: String, maxRetries: Int): String =
  {
    val saved = spinlockString(key, maxRetries)
    saved match {
      case null => ""
      case _ => saved
    }
  }

  def count(key: String, value: Int): Unit =
  {
    MemCache.set(key, value.toString)(client)
  }

  def voidCount(key: String): Unit =
  {
    MemCache.replace(key, "-999")(client)
  }

  def counted(key: String): Int =
  {
    counted(key, 5)
  }

  def counted(key: String, maxRetries: Int): Int =
  {
    val count = spinlockString(key, maxRetries)
    count match {
      case null => 0
      case _ => Integer.parseInt(count)
    }
  }

  def spinlockString(key: String, maxRetries: Int): String =
  {
    var result = MemCache.get[ String ](key)(client)
    var retries = 0
    while ( result == null && retries < maxRetries ) {
      Thread.sleep(1000)
      retries = retries + 1
      result = MemCache.get[ String ](key)(client)
    }
    result
  }

}

