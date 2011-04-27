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

case class StateLift[+A](
  sfn : Int => ( Int, Option[A] )
) extends Lift[A]

class LiftLM[A]( )
extends ForNotationAdapter[Lift,A] 
with BMonad[Lift]
with MonadFilter[Lift] {  
  override def unit [S] ( s : S ) : Lift[S] = 
    StateLift[S]( ( n : Int ) => ( n, Some( s ) ) )
  override def bind [S,T] ( ls : Lift[S], f : S => Lift[T] ) : Lift[T]
  = {
    ls match {
      case Bottom => Bottom
      case Lifted( a ) => f( a )
      case StateLift( sfn ) => {
	StateLift(
	  ( n ) => {
	    sfn( n ) match {
	      case ( np, Some( s ) ) => {
		f( s ) match {
		  case Bottom => {
		    ( np, None )
		  }
		  case Lifted( a ) => {
		    ( np, Some( a ) )
		  }
		  case StateLift( tfn ) => {
		    tfn( np )
		  }
		}
	      }
	      case ( np, None ) => ( np, None )
	    }
	  }
	)
      }
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
      case StateLift( sfn ) => {
	StateLift(
	  ( n : Int ) => {
	    val ( np, os ) = sfn( n )
	    os match {
	      case Some( s ) => {
		pred( s ) match {
		  case true => ( np, Some( s ) );
		  case false => ( np, None )
		}
	      }
	      case None => {
		( np, None )
	      }
	    }
	  }
	)
      }
    }
  }
}



