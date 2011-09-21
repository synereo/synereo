// -*- mode: Scala;-*- 
// Filename:    Continuation.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep 19 09:24:56 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

trait ParametricMonadScope[B,C] {
  class Continuation[A,B,C]( val k : ( A => B ) => C ) {
    def map( f : A => B ) : C = k( f )
    def foreach( f : A => B ) : C = map( f )
    override def equals( o : Any ) : Boolean = {
      o match {
	case that : Continuation[A,B,C] => {
	  k.equals( that.k )
	}
      }
    }
    override def hashCode( ) : Int = {
      37 * k.hashCode
    }
  }
  object Continuation {
    def apply [A,B,C] (
      k : ( A => B ) => C
    ) : Continuation[A,B,C] = {
      new Continuation[A,B,C]( k )
    }
    def unapply [A,B,C] (
      kc : Continuation[A,B,C]
    ) : Option[( ( A => B ) => C )] = {
      Some( ( kc.k ) )
    }
  }  
  
  class ContinuationM( )
  extends PMonad[Continuation] {
    override def fmap [S1,S2,T,U] (
      f : S1 => S2
    ) : Continuation[S1,T,U] => Continuation[S2,T,U] = {
      ( ctxt : Continuation[S1,T,U] ) => {	
	Continuation[S2,T,U](
	  ( s22t : S2 => T ) => {
	    for( s1 <- ctxt ) yield { s22t( f( s1 ) ) }
	  }
	)
      }
    }
    override def unit [S,T] ( s : S ) : Continuation[S,T,T] = {
      Continuation[S,T,T]( ( k : S => T ) => k( s ) )
    }
    override def mult [S,T,U,V] (      
      kks : Continuation[Continuation[S,V,U],U,T]
    ) : Continuation[S,V,T] = {      
      Continuation[S,V,T]( 
	( s2v : S => V ) => {
	  kks.map(
	    ( csvu : Continuation[S,V,U] ) => {
	      csvu.map( s2v )
	    }
	  )
	}
      )
    }
    override def strength [S1,S2,T,U](
      s1 : S1, cs2tu : Continuation[S2,T,U] 
    ) : Continuation[Tuple2[S1,S2],T,U] = {
      Continuation[Tuple2[S1,S2],T,U] (
	( s1s22t : (Tuple2[S1,S2] => T) ) => {
	  cs2tu.map(
	    ( s2 : S2 ) => {
	      val s1s2 : Tuple2[S1,S2] = ( s1, s2 )
	      s1s22t( s1s2 )
	    }
	  )
	}
      )
    }
  }
}
