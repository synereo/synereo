// -*- mode: Scala;-*- 
// Filename:    K.scala 
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 15:12:13 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object KMonad {
  import MonadicEvidence._
  case class K[A,R]( k : ( A => R ) => R ) {
    def apply( f : A => R ) : R = k( f )
  }  

  implicit def KFunctor[R]() : Functor[({type L[A] = K[A,R]})#L] =
    new Functor[({type L[A] = K[A,R]})#L] {
      def fmap[V, P >: V, U]( f : P => U ) : K[P,R] => K[U,R] = {
	( k : K[P,R] ) => {
	  new K[U,R](
	    ( nk : U => R ) => {
              k( ( p : P ) => nk( f( p ) ) )
	    }
	  )
	}
      }
    }

  implicit def KMonad[R]() : Monad[({type L[A] = K[A,R]})#L] =
    new Monad[({type L[A] = K[A,R]})#L] {      
      def apply[A]( data : A ) = new K(( k : A => R ) => k( data ) )      
      def flatten[A]( m : K[K[A,R],R] ) : K[A,R] =
        new K[A,R](
	  ( k : A => R ) => {
	    m( ( kk : K[A,R] ) => kk( k ) )
	  }
        )
    }

  trait DelimitedK[R] {
    def shift[A]( h : K[A,K[R,R]] ) : K[A,R] = {
      new K[A,R](
        ( k : A => R ) => {
          h(
            ( a : A ) => {
              new K[R,R]( ( r : R => R ) => r( k( a ) ) )
            }
          )( ( r : R ) => r )            
        }
      )
    }

    def reset( k : K[R,R] ) : K[R,R] = {
      new K[R,R](
        ( nk : R => R ) => {
          k( ( r : R ) => r )
        }
      )
    }
  }

  implicit def DKMonad[R]() : Monad[({type L[A] = K[A,R]})#L] with DelimitedK[R] =
    new Monad[({type L[A] = K[A,R]})#L] with DelimitedK[R] {      
      val kmonad = KMonad[R]()
      def apply[A]( data : A ) = kmonad( data )
      def flatten[A]( m : K[K[A,R],R] ) : K[A,R] = kmonad.flatten( m )
    }
}
