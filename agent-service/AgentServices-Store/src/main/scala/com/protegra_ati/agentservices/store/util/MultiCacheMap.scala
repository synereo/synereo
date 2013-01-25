/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.store.util

import java.util.ArrayList
import scala.collection.JavaConversions._

class MultiCacheMap[ V <: java.io.Serializable ](final val prefix: String) extends java.io.Serializable
{
  //  private var _map = new HashMap[K, List[V]]()

  def add(key: java.lang.String, value: V) =
  {
    val multiKey = prefix + key
    val cached = MemCache.get[ ArrayList[ V ] ](multiKey)(Results.client)
    cached match {
      case null => {
        val list = new ArrayList[ V ]()
        list.add(value)
        MemCache.set(multiKey, list)(Results.client)
      }
      case x: ArrayList[ V ] => {
        x.add(value)
        MemCache.replace(multiKey, x)(Results.client)
      }
      case _ => { System.out.println(" MultiCacheMap couldn't find an ArrayList for listener " + key)}
    }
    //    _map.put(key, list ++ List(value))
  }

  def remove(key: java.lang.String, value: V)
  {
    val multiKey = prefix + key
    //    if (_map.get(key) != null) {
    //         _map.get(key) -- List(value)
    //       }
    val list = get(multiKey)
    list match {
      case x: ArrayList[ V ] => {
        x.remove(value)
        MemCache.replace(multiKey, x)(Results.client)
      }
      case _ => { System.out.println(" MultiCacheMap couldn't find an ArrayList to remove item for listener " + key)}
    }
  }

  def get(key: java.lang.String): ArrayList[ V ] =
  {
    val multiKey = prefix + key
    //    _map.get(key)
    val list = MemCache.get[ ArrayList[ V ] ](multiKey)(Results.client)
    list match {
      case null => new ArrayList[V]()
      case _ => list
    }
  }

  def hasValue(key: java.lang.String, value: V): Boolean =
  {
    val multiKey = prefix + key
    val values = get(multiKey)
    ( values != null && values.contains(value) )
  }
}

