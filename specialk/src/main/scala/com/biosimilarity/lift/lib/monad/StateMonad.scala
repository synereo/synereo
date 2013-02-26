// -*- mode: Scala;-*- 
// Filename:    StateMonad.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 25 14:37:37 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

// This encoding is cleaner
object StateMonad {
  trait Functor[M[_]] {
    def map[S,P >: S, T]( f : P => T ): M[P] => M[T]
  }
  
  trait Monad[M[_]] {      
    def apply[S]( data : S ) : M[S]      
    def flatten[S]( m : M[M[S]]) : M[S]      
    final def flatMap [S, P >: S, T](
      source : M[P]
    )( t : P => M[T] )( implicit f : Functor[M] ) : M[T] = 
      flatten( f.map( t )( source ) )
  }
  case class State[S, +V]( f : ( S ) => ( S, V ) ) {
    def apply( s : S ) : ( S, V ) = f( s )
  }
  
  implicit def stateFunctor[S]() =
    new Functor[({type λ[α] = State[S,α]})#λ] {
      def map[V, P >: V, U]( f : P => U ) : State[S,P] => State[S, U] = {
	( source : State[S,P] ) => {
	  new State[S, U](
	    ( s : S ) => {
	      val ( state, value ) = source( s )
	      ( state, f( value ) )
	    }
	  )
	}
      }
    }
  
  implicit def stateMonad[S]() =  new Monad[({type λ[α] = State[S,α]})#λ]{      
    def apply[V]( data : V ) = new State(( s : S ) => ( s, data ) )      
    def flatten[V]( m : State[S, State[S, V]] ) : State[S, V] =
      new State[S, V](
	( s : S ) => {
	  val ( sp, mp ) = m( s )
	  mp( sp )
	}
      )
  }
  
  implicit def stateToComprehension[S, V]( state : State[S,V] ) = new {
    implicit val functor = stateFunctor[S]()
    val monad = stateMonad[S]()
    def map[U]( f : V => U ) = functor.map( f )( state )      
    def foreach( f : V => Unit ) = functor.map( f )( state )
    def flatMap[U]( f : V => State[S, U]) =
      monad.flatMap( state )( f )
  }
  
}
