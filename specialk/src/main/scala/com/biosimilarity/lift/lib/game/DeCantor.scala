// -*- mode: Scala;-*- 
// Filename:    DeCantor.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jun 19 21:42:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.collection
import scala.collection._
import scala.collection.generic._
import scala.collection.immutable.HashMap

class DeCantor[+A]( val contents : HashMap[Int,A] )
extends IterableProxy[A] {
  override def self : Iterable[A] = contents.values  
  def ++ [B >: A] (that: GenTraversableOnce[B]): DeCantor[B] = {
    var rslt : HashMap[Int,B] = contents
    that.foreach(
      ( t : B ) => {
	val ht = t.hashCode
	rslt = rslt + ( ( ht, t ) )	
      }
    )
    new DeCantor[B]( rslt )
  }
}

object DeCantor {
  lazy val _empty = new DeCantor[Any]( new HashMap[Int,Any]() )
  def empty [A] = _empty.asInstanceOf[DeCantor[A]]
  def apply [A] ( aS : A* ) : DeCantor[A] = {
    var rslt = new HashMap[Int,A]()
    aS.toList.foreach(
      ( t : A ) => {
	val ht = t.hashCode
	rslt = rslt + ( ( ht, t ) )
      }
    )
    new DeCantor( rslt )
  }
}
