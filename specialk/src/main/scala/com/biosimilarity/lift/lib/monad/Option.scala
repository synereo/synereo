// -*- mode: Scala;-*- 
// Filename:    Option.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 12 14:21:41 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object OptionMonad {
  import MonadicEvidence._
  
  implicit def optionFunctor() : Functor[Option] =
    new Functor[Option] {
      def fmap[V, P >: V, U]( f : P => U ) : Option[P] => Option[U] = {
	( optP : Option[P] ) => {
	  optP match {
            case Some( p ) => Some( f( p ) )
            case None => None
          }
	}
      }
    }

  implicit def optionMonad() =  new Monad[Option]{      
    def apply[V]( data : V ) = Some( data )
    def flatten[V]( m : Option[Option[V]] ) : Option[V] =
      m match {
        case Some( Some( v ) ) => Some( v )
        case _ => None
      }
  }
}
