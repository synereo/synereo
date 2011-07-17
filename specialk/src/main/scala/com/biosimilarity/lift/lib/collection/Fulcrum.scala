// -*- mode: Scala;-*- 
// Filename:    Fulcrum.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jul 17 07:49:26 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.collection

class Fulcrum[A](
  override val _1 : A,
  override val _2 : List[Fulcrum[_]],
  override val _3 : A
) extends ( A, List[Fulcrum[_]], A )( _1, _2, _3 )

object Fulcrum {
  def apply [A] (
    left : A,
    middle : List[Fulcrum[_]],
    right : A
  ) : Fulcrum[A] = {
    new Fulcrum[A]( left, middle, right )
  }
  def unapply [A] (
    flcrm : Fulcrum[A]
  ) : Option[ ( A, List[Fulcrum[_]], A ) ] = {
    Some( ( flcrm._1, flcrm._2, flcrm._3 ) )
  }
}

case class FulcrumAlt2[A,B] (
  choice : Either[( A, List[FulcrumAlt2[B,A]], A ),
		  ( B, List[FulcrumAlt2[A,B]], B )]
)

case class FulcrumAlt3[A,B,C] (
  choice : Either[( A, List[Either[FulcrumAlt3[B,C,A],FulcrumAlt3[C,B,A]]], A ),
		  Either[( B, List[Either[FulcrumAlt3[A,C,B],FulcrumAlt3[C,A,B]]], B ),
			 ( C, List[Either[FulcrumAlt3[B,A,C],FulcrumAlt3[A,B,C]]], B )]]
) 
