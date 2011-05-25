// -*- mode: Scala;-*- 
// Filename:    GenConCalc.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 08:59:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game.mnd
import com.biosimilarity.lift.lib.monad._

trait MonadicConwayCalculatorScope[
  MPlus[M[_]] <: BMonad[M] with MonadPlus[M],M[+A]
] extends MonadicConwayGameScope[MPlus,M] {
  // Generators
  import Games._
  trait GenConOps[+A] {
    def add [A1 >: A] (
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1]
    def minus [A1 >: A] (
      g : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1]
    def multiply [A1 >: A] (
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1]  
  }
  
  /*
   trait LazyGenConOps[V,+A] {
   def let(
   v : V,
   g1 : GeneralizedConwayGame[A],
   g2 : GeneralizedConwayGame[A]
   ) : GeneralizedConwayGame[A]
   }
   */
  
  // Relations
  trait GenConRelations[+A] {
    def `=G=` [A1 >: A] ( 
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : Boolean
    def `|G>` [A1 >: A] ( 
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : Boolean
    def `||` [A1 >: A] ( 
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : Boolean
  }
  
  class GenConCalculator[+A] extends GenConOps[A] {
    override def add [A1 >: A] (
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1] = {
      g1 match {
	case EmptyGenConGame => g2
	case _ => {
	  g2 match {
	    case EmptyGenConGame => EmptyGenConGame
	    case _ => {
	      val fL =
		witness.fmap(
		  ( g1L : Either[A1,GeneralizedConwayGame[A1]] ) => {
		    Right( 
		      g1L match {
			case Right( g1LG ) => {
			  add( g1LG, g2 )			
			}
			case Left( a ) => GCGSumLeft( a, g2 )
		      }
		    )
		  }
		)
	      val fR = 
		witness.fmap(
		  ( g2L : Either[A1,GeneralizedConwayGame[A1]] ) => {
		    Right(
		      g2L match {
			case Right( g2LG ) => {
			  add( g1, g2LG )
			}
			case Left( a ) => GCGSumRight( g1, a )
		      }
		    )
		  }
		)
	      val gL = 
		witness.fmap(
		  ( g1R : Either[A1,GeneralizedConwayGame[A1]] ) => {
		    Right(
		      g1R match {
			case Right( g1RG ) => {
			  add( g1RG, g2 )
			}
			case Left( a ) => GCGSumLeft( a, g2 )
		      }
		    )
		  }
		)
	      val gR = 
		witness.fmap(
		  ( g2R : Either[A1,GeneralizedConwayGame[A1]] ) => {
		    Right(
		      g2R match {
			case Right( g2RG ) => {
			  add( g1, g2RG )
			}
			case Left( a ) => GCGSumRight( g1, a )
		      }
		    )
		  }
		)

	      GenConGame(
		witness.plus(
		    fL( g1.left ),
		    fR( g2.left )
		  ),
		witness.plus(
		    gL( g1.right ),		    
		    gR( g2.right )
		)
	      )
	    }
	  }
	}
      }
    }
    override def minus [A1 >: A] (
      g : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1] = {
      def negate( gC : Either[A1,GeneralizedConwayGame[A1]] ) = {
	Right(
	  gC match {
	    case Right( h ) => minus( h )
	    case Left( a ) => GCGNegation( a )
	  }
	)
      }
      
      GenConGame(
	(witness.fmap( negate ))( g.right ),
	(witness.fmap( negate ))( g.left )
      )
    }
    override def multiply [A1 >: A] (
      g1 : GeneralizedConwayGame[A1],
      g2 : GeneralizedConwayGame[A1]
    ) : GeneralizedConwayGame[A1] = {      
      val mnd = witness[A1]
      import mnd._
      def timesLeft(
	aOrG : Either[A1,GeneralizedConwayGame[A1]],
	g : GeneralizedConwayGame[A1]
      ) : GeneralizedConwayGame[A1] = {
	aOrG match {
	  case Right( h ) => multiply( h, g )
	  case Left( a ) => GCGScalarMultipleLeft( a, g )
	}
      }
      def timesRight(      
	g : GeneralizedConwayGame[A1],
	aOrG : Either[A1,GeneralizedConwayGame[A1]]
      ) : GeneralizedConwayGame[A1] = {
	aOrG match {
	  case Right( h ) => multiply( g, h )
	  case Left( a ) => GCGScalarMultipleRight( g, a )
	}
      }
      def timesMiddle(      
	aOrG1 : Either[A1,GeneralizedConwayGame[A1]],
	aOrG2 : Either[A1,GeneralizedConwayGame[A1]]
      ) : GeneralizedConwayGame[A1] = {
	aOrG1 match {
	  case Right( g1 ) => {
	    aOrG2 match {
	      case Right( g2 ) => {
		multiply( g1, g2 )
	      }
	      case Left( a ) => {
		GCGScalarMultipleRight( g1, a )
	      }
	    }
	  }
	  case Left( a1 ) => {	  
	    aOrG2 match {
	      case Right( g2 ) => {
		GCGScalarMultipleLeft( a1, g2 )
	      }
	      case Left( a2 ) => {
		GCGScalarMultipleMiddle( a1, a2 )
	      }
	    }
	  }
	}
      }
      
      def mComp(
	g1C : Either[A1,GeneralizedConwayGame[A1]],
	g2C : Either[A1,GeneralizedConwayGame[A1]]
      ) = {
	Right(
	  add(
	    add(
	      timesLeft( g1C, g2 ),
	      timesRight( g1, g2C )
	    ),
	    minus( timesMiddle( g1C, g2C ) )
	  )
	)
      }                

      GenConGame(
	witness.plus(
	  for( g1C <- g1.left; g2C <- g2.left )
	  yield { mComp( g1C, g2C ) },
	  for( g1C <- g1.right; g2C <- g2.right )
	  yield { mComp( g1C, g2C ) }
	),
	witness.plus(
	  for( g1C <- g1.left; g2C <- g2.right )
	  yield { mComp( g1C, g2C ) },
	  for( g1C <- g1.right; g2C <- g2.left )
	  yield { mComp( g1C, g2C ) }
	)
      )
    }
  }
}
