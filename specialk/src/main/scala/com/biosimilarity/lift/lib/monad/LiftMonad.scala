// -*- mode: Scala;-*- 
// Filename:    LiftMonad.scala 
// Authors:     lgm                                                    
// Creation:    Sat Mar 12 17:29:26 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

// A structure isomorphic to Option
trait Lift[+A]
case object Bottom extends Lift[Nothing]
case class Lifted[+A]( a : A ) extends Lift[A]


// An interpretation of this structure as a monad
class LiftMonad[A]( )
extends ScalaMonadAdapter[Lift,A] 
with Monad[Lift] {  
  override def fmap [S,T]( f : S => T ) : Lift[S] => Lift[T] = {
    ( la : Lift[S] ) => {
      la match {
	case Lifted( s ) => Lifted( f( s ) )
	case _ => Bottom
      }
    }
  }
  override def unit [S] ( s : S ) : Lift[S] = Lifted[S]( s )
  override def mult [S] ( lls : Lift[Lift[S]] ) : Lift[S] = {
    lls match {
      case Lifted( Lifted( s ) ) => Lifted( s )
      case _ => Bottom
    }
  }
}

class LiftM[A]( )
extends ForNotationAdapter[Lift,A] 
with BMonad[Lift]
with MonadFilter[Lift] {  
  override def unit [S] ( s : S ) : Lift[S] = 
    Lifted[S]( s )
  override def bind [S,T] ( ls : Lift[S], f : S => Lift[T] ) : Lift[T]
  = {
    ls match {
      case Bottom => Bottom
      case Lifted( s ) => f( s )
    }
  }
  def mfilter [S] ( ls : Lift[S], pred : S => Boolean ) : Lift[S] = {
    ls match {
      case Bottom => Bottom
      case Lifted( s ) =>
	pred( s ) match {
	  case true => Lifted( s )
	  case false => Bottom
	}
    }
  }
}



