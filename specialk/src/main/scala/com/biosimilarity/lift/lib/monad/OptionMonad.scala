// -*- mode: Scala;-*- 
// Filename:    OptionMonad.scala 
// Authors:     lgm                                                    
// Creation:    Sat Mar 12 19:23:34 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

// Option interpreted as a monad
class OptionMonad[A]( )
extends ScalaMonadAdapter[Option,A] 
with Monad[Option] {  
  override def fmap [S,T]( f : S => T ) : Option[S] => Option[T] = {
    ( la : Option[S] ) => {
      la match {
	case Some( s ) => Some( f( s ) )
	case _ => None
      }
    }
  }
  override def unit [S] ( s : S ) : Option[S] = Some[S]( s )
  override def mult [S] ( lls : Option[Option[S]] ) : Option[S] = {
    lls match {
      case Some( Some( s ) ) => Some( s )
      case _ => None
    }
  }
}
