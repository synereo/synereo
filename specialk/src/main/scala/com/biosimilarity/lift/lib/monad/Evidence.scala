// -*- mode: Scala;-*- 
// Filename:    Trampoline.scala
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 04:44:46 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object MonadicEvidence {
  // The claim that a parametric type, C[_], is a Functor requires the
  // claimant to produce a definition for fmap. When we can produce
  // such evidence we say C enjoys functoriality, or C is functorial.
  trait Functor[C[_]] {
    def fmap[S,P >: S, T]( f : P => T ): C[P] => C[T]
  }
  
  // The claim that a parametric type, C[_], is a Monad requires the
  // claimant to produce definitions for apply and flatten which can
  // be used to provide canonical definitions for fmap (hence
  // demonstrating C is also a Functor) and bind. When we can produce
  // such evidence we say C enjoys monadicity, or C is monadic.
  trait Monad[C[_]] extends Functor[C] {      
    def apply[S]( data : S ) : C[S]      
    def flatten[S]( m : C[C[S]]) : C[S]    
    final def fmap[S,P >: S, T]( f : P => T ): C[P] => C[T] = {
      ( mp : C[P] ) => bind( mp )( ( p : P ) => apply[T]( f( p ) ) )
    }    
    final def bind[S, P >: S, T](
      mp : C[P]
    )( t : P => C[T] ) : C[T] = {
      flatten( fmap( t )( mp ) )
    }    
  }       
  
  // Evidence for M's monadicity can be used to produce a canonical
  // interpretation of for-comprehension syntax.
  implicit def monadToComprehension[M[C[_]] <: Monad[C],C[_],V](
    cv : C[V]
  )( implicit monad : M[C] ) = new {
    def map[U]( f : V => U ) = monad.fmap( f )( cv )
    def foreach( f : V => Unit ) = monad.fmap( f )( cv )
    def flatMap[U]( f : V => C[U]) = monad.bind( cv )( f )
  }
}

object MonadPlusEvidence {
  import MonadicEvidence._

  // A monad that supports an additional monoidal structure, namely, plus, a
  // way of combining two instances, and an identity for the plus is
  // called, imaginatively enough, MonadPlus.

  trait MonadPlus[C[_]] {
    self : Monad[C] =>
    def zero[A] : C[A]
    def plus[A]( ca1 : C[A] )( ca2 : C[A] ) : C[A]
  }
}

object FilteredMonadEvidence {
  import MonadicEvidence._
  
  // A monad that supports a means to filter its elements via a
  // predicated will be called a FilteredMonad.

  trait FilteredMonad[C[_]] {
    self : Monad[C] =>
    def filter[A]( ca : C[A] )( pred : A => Boolean ) : C[A]      
  }

  implicit def monadToFilteredComprehension[M[C[_]] <: Monad[C] with FilteredMonad[C],C[_],V](
    cv : C[V]
  )( implicit fmonad : M[C] ) = new {
    def map[U]( f : V => U ) = fmonad.fmap( f )( cv )
    def foreach( f : V => Unit ) = fmonad.fmap( f )( cv )
    def flatMap[U]( f : V => C[U]) = fmonad.bind( cv )( f )
    def withFilter( pred : V => Boolean ) = fmonad.filter( cv )( pred )
  }
}

