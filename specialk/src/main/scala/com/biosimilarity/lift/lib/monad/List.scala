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
  
  implicit def listFunctor() : Functor[List] =
    new Functor[List] {
      def fmap[V, P >: V, U]( f : P => U ) : List[P] => List[U] = {
	( listP : List[P] ) => {
	  listP.map( f )
	}
      }
    }

  implicit def listMonad() =  new Monad[List]{      
    def apply[V]( data : V ) = List( data )
    def flatten[V]( llv : List[List[V]] ) : List[V] =
      llv.flatMap( ( v ) => v )
  }
}
