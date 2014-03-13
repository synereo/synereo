// -*- mode: Scala;-*- 
// Filename:    List.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 12 14:30:45 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object ListMonad {
  import MonadicEvidence._
  import MonadPlusEvidence._
  import FilteredMonadEvidence._
  
  implicit def listMonad() = new Monad[List] with FilteredMonad[List] with MonadPlus[List] {      
    def apply[V]( data : V ) = List( data )
    def flatten[V]( llv : List[List[V]] ) : List[V] =
      llv.flatMap( ( v ) => v )
    def filter[A]( la : List[A] )( pred : A => Boolean ) : List[A] =
      la.filter( pred )
    def zero[A] = List[A]( )
    def plus[A]( ma1 : List[A] )( ma2 : List[A] ) : List[A] =
      ma1 ++ ma2
  }
}
