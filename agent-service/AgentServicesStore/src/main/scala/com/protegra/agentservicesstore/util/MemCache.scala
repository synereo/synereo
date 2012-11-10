package com.protegra.agentservicesstore.util

import net.spy.memcached.MemcachedClient
import java.net.InetSocketAddress

object MemCache
{
  def add(key: String, value : java.io.Serializable)(client: MemcachedClient) =
  {
    //expiry in seconds
    //default 3h for just in case to keep size from containing stale
    client.set(key, 10800, value);
  }

  def add(key: String, value : java.io.Serializable, expiry: Int)(client: MemcachedClient) =
  {
    client.set(key, expiry, value);
  }

  def replace(key: String, value : java.io.Serializable)(client: MemcachedClient) =
  {
    //expiry in seconds
    //default 3h for just in case to keep size from containing stale
    client.replace(key, 10800, value);
  }

  def replace(key: String, value : java.io.Serializable, expiry: Int)(client: MemcachedClient) =
  {
    client.replace(key, expiry, value);
  }

  //  def remove(key:String) {
  //  }

  def hasValue(key: String)(client: MemcachedClient): Boolean = {
    val matching = client.get(key)
    (matching != null)
  }

  def get[ T <: java.io.Serializable ](key: String)(client: MemcachedClient): T =
  {
    val matching = client.get(key)
    matching match {
      case x: T => x
      case _ => null.asInstanceOf[ T ]
    }
  }
}

