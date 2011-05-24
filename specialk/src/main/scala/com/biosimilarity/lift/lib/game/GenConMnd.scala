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

package com.biosimilarity.lift.lib.game.mnd

import com.biosimilarity.lift.lib.monad._

trait MonadicScope[MPlus[M[_]] <: BMonad[M] with MonadPlus[M],M[+A]] {
  def witness [A] : MPlus[M] with ForNotationAdapter[M,A]

  trait GeneralizedConwayGame[+A] {
    def left : M[Either[A,GeneralizedConwayGame[A]]]
    def right : M[Either[A,GeneralizedConwayGame[A]]]
  }

  case object EmptyGenConGame
  extends GeneralizedConwayGame[Nothing]
  {
    override def left : M[Either[Nothing,GeneralizedConwayGame[Nothing]]] =
      witness.zero
    override def right : M[Either[Nothing,GeneralizedConwayGame[Nothing]]] = 
      witness.zero
  }

  case class GenConGame[+A](
    left : M[Either[A,GeneralizedConwayGame[A]]],
    right : M[Either[A,GeneralizedConwayGame[A]]]
  ) extends GeneralizedConwayGame[A]
  
  // This class arises when we want to negate games. What happens when
  // we want to negate an A?
  trait GCGFormalNegation[+A]
  case class GCGNegation[+A]( a : A )
       extends GeneralizedConwayGame[A]
       with GCGFormalNegation[A]
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( a ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.zero
    }  
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
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( a ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Right( g ) )
    }  
  }
  case class GCGSumRight[+A](
    g : GeneralizedConwayGame[A],
    a : A  
  ) extends GeneralizedConwayGame[A] 
       with GCGFormalSum[A]
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Right( g ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( a ) )
    }
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
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( a ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Right( g ) )
    }
  }
  
  case class GCGScalarMultipleRight[+A](
    g : GeneralizedConwayGame[A],
    a : A
  ) extends GeneralizedConwayGame[A] 
       with GCGScalarMultiple[A]
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Right( g ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( a ) )
    }
  }
  
  case class GCGScalarMultipleMiddle[+A](
    leftA : A,
    rightA : A
  ) extends GeneralizedConwayGame[A] 
       with GCGScalarMultiple[A]
  {
    def left : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( leftA ) )
    }
    def right : M[Either[A,GeneralizedConwayGame[A]]] = {
      witness.unit( Left( rightA ) )
    }
  }
  
}

trait MonadicConwayGameScope[
  MPlus[M[_]] <: BMonad[M] with MonadPlus[M],M[+A]
] {
  type MCGScope <: MonadicScope[MPlus,M]
  def protoGames : MCGScope
  val Games : MCGScope = protoGames
}
