// -*- mode: Scala;-*- 
// Filename:    PMembrane.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep 26 14:24:04 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

trait ForNotationPShiv[Shape[+_,_,_],A,B,C] {
  self : PMonad[Shape] =>
    type ForNotationPTrampoline[A] <: PMembrane[A,B,C]
    // One approach to trampolining to Scala's for-notation is
    // presented below. We provide an Option-like structure, called
    // Membrane, which represents the basic interface to
    // for-notation. Then we provide the monadic layer in terms of
    // this.
    trait PMembrane[+A,U,T] {
      def inhabitant : Either[A,Shape[A,U,T]]
      def flatMap [B,V] (
	f : A => PMembrane[B,V,U]
      )(
      implicit
	u2t : PMembrane[B,V,U] => PMembrane[B,V,T],
	v2t : Shape[B,V,V] => Shape[B,V,U]
    ) : PMembrane[B,V,T]
      def foreach ( f : A => Unit ) : Unit
      def map [B] ( f : A => B ) : PMembrane[B,U,T]
    }  

  // case object POpen extends PMembrane[Nothing,Nothing,Nothing] {
//     override def flatMap [B,V] (
//       f : Nothing => PMembrane[B,V,Nothing]
//     )(
//       implicit
// 	u2t : PMembrane[B,V,Nothing] => PMembrane[B,V,Nothing],
// 	v2t : PMembrane[B,V,V] => PMembrane[B,V,Nothing]
//     ) : PMembrane[B,V,Nothing] = { u2t( this ) }
//     override def foreach ( f : Nothing => Unit ) : Unit = { }
//     override def map [B] (
//       f : Nothing => B
//     ) : PMembrane[B,Nothing,Nothing] = { this }
//   }

  case class PCell[+A,U,T]( a : A ) extends PMembrane[A,U,T] {
    override def inhabitant : Either[A,Shape[A,U,T]] =
      Left[A,Shape[A,U,T]]( a )
    override def flatMap [B,V] (
      f : A => PMembrane[B,V,U]
    )(
      implicit
	u2t : PMembrane[B,V,U] => PMembrane[B,V,T],
	v2t : Shape[B,V,V] => Shape[B,V,U]
    ) : PMembrane[B,V,T] = { u2t( f( a ) ) }
    override def foreach ( f : A => Unit ) : Unit = { f( a ) }
    override def map [B] ( f : A => B ) : PMembrane[B,U,T] = { PCell( f( a ) ) }
  }  

  // Now, we special case the wrapping in PMembrane's of Shape's -- for
  // which we have provided a monadic interpretation -- as witnessed
  // by the self-type above.

  case class SPCell[A,U,T]( val sa : Shape[A,U,T] ) extends PMembrane[A,U,T]
  {
    override def inhabitant : Either[A,Shape[A,U,T]] =
      Right[A,Shape[A,U,T]]( sa )
    override def flatMap [B,V] (
      f : A => PMembrane[B,V,U]
    )(
      implicit
	u2t : PMembrane[B,V,U] => PMembrane[B,V,T],
	v2t : Shape[B,V,V] => Shape[B,V,U]
    ) : PMembrane[B,V,T] = {
      new SPCell[B,V,T](
	bind[A,B,T,U,V](
	  sa,
	  ( a : A ) => {
	    f( a ) match {
	      //case POpen => throw new Exception( "Encountered open cell" )
	      case SPCell( sb : Shape[B,V,U] ) => sb
	      case PCell( b ) => v2t( unit[B,V]( b ) )
	    }
	  }
	)
      )
    }

    override def foreach ( f : A => Unit ) : Unit = {
      bind[A,Unit,T,U,Unit](
	sa,
	( a : A ) => unit[Unit,U]( f( a ) ).asInstanceOf[Shape[Unit,Unit,U]]
      );
    }

    override def map [B] (
      f : A => B
    ) : PMembrane[B,U,T] = {      
      SPCell[B,U,T]( fmap[A,B,U,T]( f )( sa ) )
    }
  } 
  
}

trait ForNotationApplyPShiv[Shape[+_,_,_],A,B,C] {
  self : ForNotationPShiv[Shape,A,B,C] =>
    //override type ForNotationPTrampoline[A] = PMembrane[A,B,C]
    def apply [A,B,C] (
      sa : Shape[A,B,C]
    ) : PMembrane[A,B,C] = {
      SPCell[A,B,C]( sa )
  }
}
