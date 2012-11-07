package com.protegra.agentservicesstore.util

import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress

object MemCache
{
  def add(key: String, value : java.io.Serializable)(client: MemcachedClient) =
  {
    //expiry in seconds
    //default 3h for just in case to keep size from containing stale
    client.set(key.toString, 10800, value);
  }

  def add(key: String, value : java.io.Serializable, expiry: Int)(client: MemcachedClient) =
  {
    client.set(key.toString, expiry, value);
  }

  //  def remove(key:String) {
  //  }

  def get[ T <: java.io.Serializable ](key: String)(client: MemcachedClient): T =
  {
    val matching = client.get(key.toString)
    matching match {
      case x: T => x
      case _ => null.asInstanceOf[ T ]
    }
  }
}

