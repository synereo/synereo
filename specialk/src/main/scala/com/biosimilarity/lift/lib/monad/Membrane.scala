// -*- mode: Scala;-*- 
// Filename:    Membrane.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 25 10:20:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

trait ForNotationShiv[Shape[_],A] {
  self : BMonad[Shape] with MonadFilter[Shape] =>
    type ForNotationTrampoline[A] <: Membrane[A] with Filter[A]
    // One approach to trampolining to Scala's for-notation is
    // presented below. We provide an Option-like structure, called
    // Membrane, which represents the basic interface to
    // for-notation. Then we provide the monadic layer in terms of
    // this.
    trait Membrane[+A] {
      def flatMap [B] ( f : A => Membrane[B] ) : Membrane[B]
      def foreach ( f : A => Unit ) : Unit
      def map [B] ( f : A => B ) : Membrane[B]
    }
  
  // For adding for-notation filter behavior to the mix
  trait Filter[+A] {
    self : Membrane[A] =>
      def withFilter( pred : A => Boolean ) : Membrane[A] with Filter[A]
    def filter( pred : A => Boolean ) : Membrane[A] with Filter[A] = {
      withFilter( pred )
    }
  }

  case object Open extends Membrane[Nothing] with Filter[Nothing] {
    override def flatMap [B] ( f : Nothing => Membrane[B] ) : Membrane[B] = {
      this
    }
    override def foreach ( f : Nothing => Unit ) : Unit = {
    }
    override def map [B] ( f : Nothing => B ) : Membrane[B] = {
      this
    }
    override def withFilter(
      pred : Nothing => Boolean
    ) : Membrane[Nothing] with Filter[Nothing] = {
      this
    }
  }

  case class Cell[+A]( a : A ) extends Membrane[A] with Filter[A] {
    override def flatMap [B] ( f : A => Membrane[B] ) : Membrane[B] = {
      for( b <- f( a ) ) yield { b }
    }
    override def foreach ( f : A => Unit ) : Unit = {
      f( a )
    }
    override def map [B] ( f : A => B ) : Membrane[B] = {
      Cell( f( a ) )
    }
    def withFilter( pred : A => Boolean ) : Membrane[A] with Filter[A] = {
      pred( a ) match {
	case true => this
	case false => Open
      }
    }
  }  

  // Up to verification of the monad laws, this verifies that Membrane
  // defines a monad, itself. In some sense all monads factor through
  // some kind of wrapper-like structure, i.e. they reflect
  // computation into a datum.
  class MembraneMonad[A]( )
  extends BMonad[Membrane] {    
    override def unit [S] ( s : S ) : Membrane[S] = 
      Cell[S]( s )
    override def bind [S,T] (
      ms : Membrane[S],
      f : S => Membrane[T]
    ) : Membrane[T] = {
      for( s <- ms; t <- f( s ) ) yield { t }
    }    
  }

  // Now, we special case the wrapping in Membrane's of Shape's -- for
  // which we have provided a monadic interpretation -- as witnessed
  // by the self-type above.

  class SCell[A]( val sa : Shape[A] )
       extends Membrane[A] with Filter[A]
  {
    override def flatMap [B] ( f : A => Membrane[B] ) : Membrane[B] = {
      SCell[B](
	bind[A,B](
	  sa,
	  ( a : A ) => {
	    f( a ) match {
	      case Open => throw new Exception( "Encountered open cell" )
	      case SCell( sb : Shape[B] ) => sb
	      case Cell( b ) => unit[B]( b )
	    }
	  }
	)
      )
    }

    override def foreach ( f : A => Unit ) : Unit = {
      bind[A,Unit]( sa,	( a : A ) => unit( f( a ) ) );
    }

    override def map [B] ( f : A => B ) : Membrane[B] = {
      SCell[B](
	bind[Shape[A],B](
	  unit[Shape[A]]( sa ),
	  fmap[A,B]( f )
	)
      )
    }

    def withFilter( pred : A => Boolean ) : Membrane[A] with Filter[A] = {
      SCell[A]( mfilter[A]( sa, pred ) )
    }

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : SCell[A] => {
	  sa.equals( that.sa )
	}
	case _ => false
      }
    }
    override def hashCode( ) : Int = {
      37 * sa.hashCode
    }
  }

  object SCell {
    def apply [A] ( sa : Shape[A] ) : SCell[A] = {
      SCell[A]( sa )
    }
    def unapply [A] ( sca : SCell[A] ) : Option[( Shape[A] )] = {
      Some( ( sca.sa ) )
    }
  } 
  
}

trait ForNotationApplyShiv[Shape[_],A] {
  self : ForNotationShiv[Shape,A] =>
    def apply [A] ( sa : Shape[A] ) : ForNotationTrampoline[A]
}

trait ForNotationStdApplyShiv[Shape[_],A] {
  self : ForNotationShiv[Shape,A] =>
    type ForNotationTrampoline[A] = SCell[A]

  def apply [A] ( sa : Shape[A] ) : ForNotationTrampoline[A] = {
    new self.SCell[A]( sa )
  }
}

trait ForNotationImplicitsShiv[Shape[_],A] {
  self : ForNotationShiv[Shape,A] =>
    type ForNotationTrampoline[A] = SCell[A]

  // Next, we provide some useful implicits:
  // One to enclose Shape's in Membrane's ...
  implicit def toMembrane [A] (
    s : Shape[A]
  ) : ForNotationTrampoline[A] = {
    self.SCell[A]( s )
  }

  // ... and one to open the enclosure
  implicit def toShape [A] ( s : Membrane[A] ) : Shape[A] = {
    s match {
      case self.SCell( sa : Shape[A] ) => sa
      case _ => throw new Exception( "value escaping enclosure" )
    }
  }  
}

trait ForNotationAdapter[Shape[_],A]
  extends ForNotationShiv[Shape,A]
  with ForNotationImplicitsShiv[Shape,A] {
    self : BMonad[Shape] with MonadFilter[Shape] =>
}

trait FNMonadT[T[M[_],_],M[_],A]
extends MonadT[T,M] {  
  trait TMSMA[A]
       extends ForNotationAdapter[TM,A]
       with BMonad[TM]
       with MonadFilter[TM]
       with MonadPlus[TM]

  def tmsma : TMSMA[A]
}
