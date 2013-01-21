/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.store.util

import java.util.HashMap

class MultiMap[K, V] extends java.io.Serializable{
  private var _map = new HashMap[K, List[V]]()

  def add(key:K, value:V) = {
    val list:List[V] = if (_map.get(key) == null) List[V]() else _map.get(key)
    _map.put(key, list ++ List(value))
  }

  def remove(key:K, value:V) {
    if (_map.get(key) != null) {
      val sublist = _map.get(key).filterNot(x => x == value)
      _map.put(key, sublist)
    }
  }

  def get(key:K):List[V] = {
    _map.get(key)
  }
  
  def hasValue(key:K, value:V):Boolean = {
    val values = get(key)
    (values != null && values.contains(value))
  }
}

