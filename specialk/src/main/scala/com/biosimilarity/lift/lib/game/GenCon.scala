// -*- mode: Scala;-*- 
// Filename:    GenCon.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 10 11:50:40 2011 
// Copyright:   Not supplied 
// Description: 
//
// The game is afoot, Watson! The idea is to solve for X in the
// relations given by Monoid : Monad :: Conway Game : X. We approach
// this problem by simply recording all maps that have to be present
// to make the field structure on Conway Games work when we allow
// atoms in Conway Games. 
//
// What does it mean to add atoms to Conway Games? Notice that like
// sets in ordinary ZF Set Theory, Conway Games are built from
// nothing, i.e. from the empty Game: { | }. Adding atoms is a bit
// like moving from ZF Set Theory to Fraenkl-Mostowski Set Theory
// where we assume a set of atoms, say A, and then allow them to occur
// in our sets. The crucial observation is that this idea is exactly
// the same as saying that sets are containers parametrically
// polymorphic in some type, A. 
//
// We might notice that this is also precisely the step in moving from
// Monoid to Monad. If we interpret the Monoid as a kind of container
// and then add atoms, and then interpret this structure as a
// container parametrically polymorphic in the type of atoms we're
// adding, we arrive at a Monad.
//
// So, we employ the same strategy for generalizing Conway Games. We
// add atoms as a type parameter. When we carry out the field
// operations on these new structures we see that we need some
// maps. The trick is to make sure the dog doesn't bark: that is, we
// simply write down the minimal requirements for the maps in order to
// satisfy the type checker of our ambient typed functional language
// and -- voila! -- we have an automated proof that our structure is a
// "minimal" solution for X.
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }

trait GeneralizedConwayGame[+A] {
  def left : Set[Either[A,GeneralizedConwayGame[A]]]
  def right : Set[Either[A,GeneralizedConwayGame[A]]]  
}

trait GenConRenderer[+A] {
  self : GeneralizedConwayGame[A] =>

  def openBrace : String = "{"
  def closeBrace : String = "}"

  def componentBound : Int = 10

  override def toString : String = {
    def compStr( comp : List[Either[A,GeneralizedConwayGame[A]]] ) = {
      comp match {
	case Nil => ""
	case l :: ls => {
	  val s =
	    l match {
	      case Right( g ) => g + ""
	      case Left( a ) => a + ""
	    }
	  ( s /: ls )(
	    ( acc, e ) => {
	      (
		acc
		+ ","
		+ (
		  e match {
		    case Right( g ) => g + ""
		    case Left( a ) => a + ""
		  }
		)
	      )
	    }
	  )
	}
      }
    }

    (
      openBrace
      + compStr( left.self.toList )
      + "|"
      + compStr( right.self.toList )
      + closeBrace
    )
  }
}

case object EmptyGenConGame
extends GeneralizedConwayGame[Nothing]
with GenConRenderer[Nothing]
{
  override def left : Set[Either[Nothing,GeneralizedConwayGame[Nothing]]] = 
    Set.empty[Either[Nothing,GeneralizedConwayGame[Nothing]]]
  override def right : Set[Either[Nothing,GeneralizedConwayGame[Nothing]]] = 
    Set.empty[Either[Nothing,GeneralizedConwayGame[Nothing]]]
}

case class GenConGame[+A](
  left : Set[Either[A,GeneralizedConwayGame[A]]],
  right : Set[Either[A,GeneralizedConwayGame[A]]]
) extends GeneralizedConwayGame[A] 
     with GenConRenderer[A]

// This class arises when we want to negate games. What happens when
// we want to negate an A?
trait GCGFormalNegation[+A]
case class GCGNegation[+A]( a : A )
extends GeneralizedConwayGame[A]
   with GCGFormalNegation[A]
   with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( a ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set.empty[Either[A,GeneralizedConwayGame[A]]]
  }  
  override def openBrace : String = "{-|"
  override def closeBrace : String = "|-}"
}

// These classes arise when we want to add games. What happens when we
// want to add an A and a GeneralizedConwayGame[A] ? We simply treat
// it as a formal sum. Note that because of the definition of add we
// will never add an A to an A.
trait GCGFormalSum[+A]
case class GCGSumLeft[+A](
  a : A,
  g : GeneralizedConwayGame[A]
) extends GeneralizedConwayGame[A]
     with GCGFormalSum[A]
     with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( a ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Right( g ) )
  }  
  override def openBrace : String = "{<|"
  override def closeBrace : String = "|<}"
}
case class GCGSumRight[+A](
  g : GeneralizedConwayGame[A],
  a : A  
) extends GeneralizedConwayGame[A] 
     with GCGFormalSum[A]
     with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Right( g ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( a ) )
  }
  override def openBrace : String = "{>|"
  override def closeBrace : String = "|>}"
}

// These classes arise when we want to multiply games. What happens when we
// want to multiply an A and a GeneralizedConwayGame[A] ? We simply treat
// it as a scalar multiple. Note that because of the definition of
// multiply we will occasionally multiply an A by an A.
trait GCGScalarMultiple[+A]
case class GCGScalarMultipleLeft[+A](
  a : A,
  g : GeneralizedConwayGame[A]
) extends GeneralizedConwayGame[A] 
     with GCGScalarMultiple[A]
     with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( a ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Right( g ) )
  }
  override def openBrace : String = "{*<|"
  override def closeBrace : String = "|*<}"
}

case class GCGScalarMultipleRight[+A](
  g : GeneralizedConwayGame[A],
  a : A
) extends GeneralizedConwayGame[A] 
     with GCGScalarMultiple[A]
     with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Right( g ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( a ) )
  }
  override def openBrace : String = "{*>|"
  override def closeBrace : String = "|*>}"
}

case class GCGScalarMultipleMiddle[+A](
  leftA : A,
  rightA : A
) extends GeneralizedConwayGame[A] 
     with GCGScalarMultiple[A]
     with GenConRenderer[A]
{
  def left : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( leftA ) )
  }
  def right : Set[Either[A,GeneralizedConwayGame[A]]] = {
    Set( Left( rightA ) )
  }
  override def openBrace : String = "{*|"
  override def closeBrace : String = "|*}"
}




