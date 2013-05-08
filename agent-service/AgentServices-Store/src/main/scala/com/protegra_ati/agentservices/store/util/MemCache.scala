package com.protegra_ati.agentservices.store.util

import net.spy.memcached.{CachedData, CASMutator, CASMutation, MemcachedClient}
import java.net.InetSocketAddress
import java.util.Collections
import net.spy.memcached.transcoders.{SerializingTranscoder, LongTranscoder, Transcoder}

class GenericTranscoder[ T <: java.io.Serializable ] extends Transcoder[ T ]
{

  val delegate = new SerializingTranscoder()

  def asyncDecode(d: CachedData): Boolean =
  {
    delegate.asyncDecode(d)
  }

  def decode(d: CachedData): T =
  {
    delegate.decode(d).asInstanceOf[ T ]
  }

  def encode(o: T): CachedData =
  {
    delegate.encode(o)
  }

  def getMaxSize(): Int =
  {
    delegate.getMaxSize()
  }
}

class ListTranscoder[ T <: java.io.Serializable ] extends Transcoder[ List[ T ] ]
{

  val delegate = new SerializingTranscoder()

  def asyncDecode(d: CachedData): Boolean =
  {
    delegate.asyncDecode(d)
  }

  def decode(d: CachedData): List[ T ] =
  {
    delegate.decode(d).asInstanceOf[ List[ T ] ]
  }

  def encode(o: List[ T ]): CachedData =
  {
    delegate.encode(o)
  }

  def getMaxSize(): Int =
  {
    delegate.getMaxSize()
  }
}

object MemCache
{
  def get[ T <: java.io.Serializable ](key: String)(client: MemcachedClient): T =
  {
    val matching = client.get(key)
    matching match {
      case x: T => x
      case _ => null.asInstanceOf[ T ]
    }
  }

  def set(key: String, value: java.io.Serializable)(client: MemcachedClient) =
  {
    //expiry in seconds
    //default 3h for just in case to keep size from containing stale
    client.set(key, 60*60*3, value)
  }

  def set(key: String, value: java.io.Serializable, expiry: Int)(client: MemcachedClient) =
  {
    client.set(key, expiry, value)
  }

  def replace(key: String, value: java.io.Serializable)(client: MemcachedClient) =
  {
    //expiry in seconds
    //default 3h for just in case to keep size from containing stale
    client.replace(key, 60*60*3, value)
  }

  def replace(key: String, value: java.io.Serializable, expiry: Int)(client: MemcachedClient) =
  {
    client.replace(key, expiry, value)
  }

  //  def remove(key:String) {
  //  }

  def hasValue(key: String)(client: MemcachedClient): Boolean =
  {
    val matching = client.get(key)
    ( matching != null )
  }

  //this uses gets, important not to confuse with get
  def getList[ T <: java.io.Serializable ](key: String)(client: MemcachedClient): List[ T ] =
  {
    val matching = client.gets(key)
    if (matching == null)
      return null

    matching.getValue match {
      case x: List[ T ] => x
      case _ => null.asInstanceOf[ List[ T ] ]
    }
  }

  //this uses cas for compare and set, a concurrency friendly version of set
  def addToList[ T <: java.io.Serializable ](key: String, value: T)(client: MemcachedClient): List[ T ] =
  {
    // This is how we modify a list when we find one in the cache.
    val mutation = new CASMutation[ List[ T ] ]()
    {
      // This is only invoked when a value actually exists.
      def getNewValue(current: List[ T ]): List[ T ] =
      {
        value :: current
      }
    }
    // The initial value -- only used when there's no list stored under the key.
    val initialValue: List[ T ] = value :: Nil

    // The mutator who'll do all the low-level stuff.
    val transcoder = new ListTranscoder[ T ]()
    val mutator = new CASMutator[ List[ T ] ](client, transcoder)

    // This returns whatever value was successfully stored within the cache -- either the initial list as above, or a mutated existing  one
    return mutator.cas(key, initialValue, 0, mutation)
  }


}

