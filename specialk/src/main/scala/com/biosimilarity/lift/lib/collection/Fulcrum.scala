// -*- mode: Scala;-*- 
// Filename:    Fulcrum.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jul 17 07:49:26 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.collection

trait Fulcrums[C[X] <: Iterable[X]] {
  type F[A] <: Fulcrum[C,A]
  type UF[A] <: UniformFulcrum[C,A]
  class Fulcrum[M[X] <: Iterable[X],A](
    override val _1 : A,
    override val _2 : M[F[_]],
    override val _3 : A
  ) extends ( A, M[F[_]], A )( _1, _2, _3 )
  
  object Fulcrum {
    def apply [M[X] <: Iterable[X],A] (
      left : A,
      middle : M[F[_]],
      right : A
    ) : Fulcrum[M,A] = {
      new Fulcrum[M,A]( left, middle, right )
    }
    def unapply [M[X] <: Iterable[X],A] (
      flcrm : Fulcrum[M,A]
    ) : Option[ ( A, M[F[_]], A ) ] = {
      Some( ( flcrm._1, flcrm._2, flcrm._3 ) )
    }
  }
  
  class UniformFulcrum[M[X] <: Iterable[X],A](
    override val _1 : A,
    override val _2 : M[UF[A]],
    override val _3 : A
  ) extends ( A, M[UF[A]], A )( _1, _2, _3 )
  
  object UniformFulcrum {
    def apply [M[X] <: Iterable[X],A] (
      left : A,
      middle : M[UF[A]],
      right : A
    ) : UniformFulcrum[M,A] = {
      new UniformFulcrum[M,A]( left, middle, right )
    }
    def unapply [M[X] <: Iterable[X],A] (
      flcrm : UniformFulcrum[M,A]
    ) : Option[ ( A, M[UF[A]], A ) ] = {
      Some( ( flcrm._1, flcrm._2, flcrm._3 ) )
    }
  }
  
  case class FulcrumAlt2[M[X] <: Iterable[X],A,B] (
    choice : Either[( A, M[FulcrumAlt2[M,B,A]], A ),
		    ( B, M[FulcrumAlt2[M,A,B]], B )]
  )
  
  case class FulcrumAlt3[M[X] <: Iterable[X],A,B,C] (
    choice : Either[( A, M[Either[FulcrumAlt3[M,B,C,A],FulcrumAlt3[M,C,B,A]]], A ),
		    Either[( B, M[Either[FulcrumAlt3[M,A,C,B],FulcrumAlt3[M,C,A,B]]], B ),
			   ( C, M[Either[FulcrumAlt3[M,B,A,C],FulcrumAlt3[M,A,B,C]]], B )]]
  ) 
  
}

trait FulcrumScope[C[X] <: Iterable[X]] {
  type Fulcrumology <: Fulcrums[C]
  val theFulcrumology : Fulcrumology = protoFulcrumology
  def protoFulcrumology : Fulcrumology 
}
