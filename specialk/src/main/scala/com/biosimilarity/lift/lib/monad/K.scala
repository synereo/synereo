// -*- mode: Scala;-*- 
// Filename:    K.scala 
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 15:12:13 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object CCMonad {
  import MonadicEvidence._

  // This version is from Wadler's Composable Continuations paper.
  
  case class CC[A,R]( k : ( A => R ) => R ) {
    def apply( f : A => R ) : R = k( f )
  }  

  implicit def CCFunctor[R]() : Functor[({type L[A] = CC[A,R]})#L] =
    new Functor[({type L[A] = CC[A,R]})#L] {
      def fmap[V, P >: V, U]( f : P => U ) : CC[P,R] => CC[U,R] = {
	( k : CC[P,R] ) => {
	  new CC[U,R](
	    ( nk : U => R ) => {
              k( ( p : P ) => nk( f( p ) ) )
	    }
	  )
	}
      }
    }

  implicit def CCMonad[R]() : Monad[({type L[A] = CC[A,R]})#L] =
    new Monad[({type L[A] = CC[A,R]})#L] {      
      def apply[A]( data : A ) = new CC(( k : A => R ) => k( data ) )      
      def flatten[A]( m : CC[CC[A,R],R] ) : CC[A,R] =
        new CC[A,R](
	  ( k : A => R ) => {
	    m( ( kk : CC[A,R] ) => kk( k ) )
	  }
        )
    }

  trait DelimitedCC[R] {
    def shift[A]( h : CC[A,CC[R,R]] ) : CC[A,R] = {
      new CC[A,R](
        ( k : A => R ) => {
          h(
            ( a : A ) => {
              new CC[R,R]( ( r : R => R ) => r( k( a ) ) )
            }
          )( ( r : R ) => r )            
        }
      )
    }

    def reset( k : CC[R,R] ) : CC[R,R] = {
      new CC[R,R](
        ( nk : R => R ) => {
          k( ( r : R ) => r )
        }
      )
    }
  }

  implicit def DCCMonad[R]() : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] =
    new Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] {      
      val kmonad = CCMonad[R]()
      def apply[A]( data : A ) = kmonad( data )
      def flatten[A]( m : CC[CC[A,R],R] ) : CC[A,R] = kmonad.flatten( m )
    }
}

object PCMonad {
  import MonadicEvidence._
  import ParametricMonadicEvidence._

  // This version is from Atkey's Parametric Monads paper.

  case class Continuation[+A,-B,+C]( val k : ( A => B ) => C ) {
    def apply( f : A => B ) : C = k( f )
    def map( f : A => B ) : C = this( f )
  }

  implicit def pcMonad[A,B,C]( c : Continuation[A,B,C] ) : PMonad[Continuation] = {
    new PMonad[Continuation] {
      def apply[S,T]( s : S ) : Continuation[S,T,T] =
        Continuation[S,T,T]( ( k : S => T ) => k( s ) )
      def flatten[S,T,U,V] (      
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
      def strength [S1,S2,T,U](
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

  trait DelimitedContinuation[X,Y,Z] {
    self : PMonad[Continuation] =>
    def reset [A,B,C] (
      c : Continuation[A,A,B]
    ) : Continuation[B,C,C] = {
      Continuation[B,C,C](
        ( b2c2c : ( B => C ) ) => {
	  b2c2c( c.map( ( a : A ) => a ) )
        }
      )
    }
    def shift [A,B,C,D,E] (
      a2cbcc2cdde : ( A => Continuation[B,C,C] ) => Continuation[D,D,E]
    ) : Continuation[A,B,E] = {
      Continuation[A,B,E](
        ( a2b : A => B ) => {
	  a2cbcc2cdde(
	    ( a : A ) => {
	      apply[B,C]( a2b( a ) )
	    }
	  ).map( ( d : D ) => d )
        }
      )
    }
  }
}
