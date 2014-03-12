// -*- mode: Scala;-*- 
// Filename:    State.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 12 14:09:06 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object StateMonad {
  import MonadicEvidence._

  case class State[S, +V]( f : ( S ) => ( S, V ) ) {
    def apply( s : S ) : ( S, V ) = f( s )
  }

  implicit def stateFunctor[S]() : Functor[({type L[A] = State[S,A]})#L] =
    new Functor[({type L[A] = State[S,A]})#L] {
      def fmap[V, P >: V, U]( f : P => U ) : State[S,P] => State[S,U] = {
	( s : State[S,P] ) => {
	  new State[S,U](
	    ( ns : S ) => {
              val ( state, value ) = s( ns )
	      ( state, f( value ) )
	    }
	  )
	}
      }
    }

  implicit def stateMonad[S]() =  new Monad[({type L[A] = State[S,A]})#L]{      
    def apply[V]( data : V ) = new State(( s : S ) => ( s, data ) ) 
    def flatten[V]( m : State[S,State[S, V]] ) : State[S,V] =
      new State[S, V](
	( s : S ) => {
	  val ( sp, mp ) = m( s )
	  mp( sp )
	}
      )
  }
}
