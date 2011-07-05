// -*- mode: Scala;-*- 
// Filename:    XMap.scala 
// Authors:     lgm                                                    
// Creation:    Sat Jul  2 12:42:59 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.collection

import scala.collection.Map
import scala.collection.MapProxyLike
import scala.collection.immutable.HashMap

class XMap[A, +B](
  override val seq : HashMap[A,B]
) extends Map[A,B] with MapProxyLike[A,B,Map[A,B]] {
  override def self = seq
  override def empty =
    new XMap[A,B]( new HashMap[A,B]( ) )
}


