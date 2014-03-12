// -*- mode: Scala;-*- 
// Filename:    Trampoline.scala
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 04:44:46 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object MonadicEvidence {
  // The claim that a parametric type, M[_], is a Functor requires the
  // claimant to produce a definition for fmap. When we can produce
  // such evidence we say M enjoys functoriality, or M is functorial.
  trait Functor[M[_]] {
    def fmap[S,P >: S, T]( f : P => T ): M[P] => M[T]
  }
  
  // The claim that a parametric type, M[_], is a Monad requires the
  // claimant to produce definitions for apply and flatten which can
  // be used to provide canonical definitions for fmap (hence
  // demonstrating M is also a Functor) and bind. When we can produce
  // such evidence we say M enjoys monadicity, or M is monadic.
  trait Monad[M[_]] extends Functor[M] {      
    def apply[S]( data : S ) : M[S]      
    def flatten[S]( m : M[M[S]]) : M[S]    
    final def fmap[S,P >: S, T]( f : P => T ): M[P] => M[T] = {
      ( mp : M[P] ) => bind( mp )( ( p : P ) => apply[T]( f( p ) ) )
    }    
    final def bind[S, P >: S, T](
      mp : M[P]
    )( t : P => M[T] ) : M[T] = {
      flatten( fmap( t )( mp ) )
    }    
  }       
  
  // Evidence for M's monadicity can be used to produce a canonical
  // interpretation of for-comprehension syntax.
  implicit def monadToComprehension[M[_],V](
    m : M[V]
  )( implicit monad : Monad[M] ) = new {
    def map[U]( f : V => U ) = monad.fmap( f )( m )
    def foreach( f : V => Unit ) = monad.fmap( f )( m )
    def flatMap[U]( f : V => M[U]) = monad.bind( m )( f )
  }
}

