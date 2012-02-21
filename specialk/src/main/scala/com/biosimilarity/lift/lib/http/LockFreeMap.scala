package com.biosimilarity.lift.lib.http

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable.{Map => MutableMap, MapLike}
import scala.collection.generic.{CanBuildFrom, MutableMapFactory}
import java.util.concurrent.atomic.AtomicReference


/**
 * a lock free map that looks like a mutable map to the outside world and internally
 * delegates to an immutable map.  Any mutations create a new immutable map and then use
 * atomic get and set (aka CAS) to mutate the delegate map.  If the atomic get and set fails it will
 * redo the creation of the ew immutable map and try again, etc, etc.  
 *
 * Performance profile is excellent for scenarios with large number of reads compared to writes/mutations.
 *
 */
class LockFreeMap[A,B] extends MutableMap[A,B] 
    with MapLike[A, B, LockFreeMap[A,B]]                                        
{
  
  private val _ref = new AtomicReference(ImmutableMap[A,B]())
  
  override def empty: LockFreeMap[A, B] = LockFreeMap.empty[A, B]
  
  def get(key: A): Option[B] = _ref.get.get(key)
  
  def iterator: Iterator[(A, B)] = _ref.get.iterator
  
  def -=(key: A): this.type = transaction{_ - key}

  def += (kv: (A, B)): this.type = transaction(_ + kv) 
  
  private def transaction(mutator: ImmutableMap[A,B]=>ImmutableMap[A,B]): this.type = {
    def tryit = {
      val original = _ref.get
	  val updated = mutator(original)
	  _ref.compareAndSet(original, updated)        
    }
    while ( !tryit ) {}
    this
  }

}


object LockFreeMap extends MutableMapFactory[LockFreeMap] {
    implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LockFreeMap[A, B]] = new MapCanBuildFrom[A, B]
    def empty[A, B]: LockFreeMap[A, B] = new LockFreeMap[A, B]
}
