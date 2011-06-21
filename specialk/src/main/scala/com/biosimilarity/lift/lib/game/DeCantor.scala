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
  // BUGBUG -- lgm: i'm not sure why zip and ++ are dispatched to
  // correctly but map isn't. So, i've renamed it to mapf.
  // BUGBUG -- lgm: this pattern is terribly inefficient, but the
  // mutable maps are invariant in the target type.
  def mapf [B] ( f : (A) => B ) : DeCantor[B] = {
    var rslt : HashMap[Int,B] = new HashMap[Int,B]()
    for( a <- self; b = f( a ) ) {
      rslt = rslt + ( ( b.hashCode, b ) )
    }
    new DeCantor( rslt )
  }
  def zip [B] (that: GenIterable[B]): DeCantor[(A, B)] = {
    var rslt : HashMap[Int,( A, B )] = new HashMap[Int,( A, B )]()
    for( ab <- self.zip( that ) ) {
      rslt = rslt + ( ( ab.hashCode, ab ) )
    }
    new DeCantor( rslt )
  }
  def ++ [B >: A] (that: GenTraversableOnce[B]): DeCantor[B] = {
    var rslt : HashMap[Int,B] = contents
    that.foreach(
      ( t : B ) => {
	rslt = rslt + ( ( t.hashCode, t ) )	
      }
    )
    new DeCantor[B]( rslt )
  }
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : DeCantor[A] => {
	contents.equals( that.contents )
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    ( 37 * contents.hashCode )
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
