// -*- mode: Scala;-*- 
// Filename:    GenConCalc.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 08:59:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }

// Generators
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
	    GenConGame(
	      (
		g1.left.mapf(
		  ( g1L ) => {
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
		++
		g2.left.mapf(
		  ( g2L ) => {
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
	      ),
	      (
		g1.right.mapf(
		  ( g1R ) => {
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
		++
		g2.right.mapf(
		  ( g2R ) => {
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

    GenConGame( g.right.mapf( negate ), g.left.mapf( negate ) )
  }
  override def multiply [A1 >: A] (
    g1 : GeneralizedConwayGame[A1],
    g2 : GeneralizedConwayGame[A1]
  ) : GeneralizedConwayGame[A1] = {
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
      gtpl : (
	Either[A1,GeneralizedConwayGame[A1]],
	Either[A1,GeneralizedConwayGame[A1]]
      )
    ) = {
      val ( g1L, g2L ) = gtpl
      Right(
	add(
	  add(
	    timesLeft( g1L, g2 ),
	    timesRight( g1, g2L )
	  ),
	  minus( timesMiddle( g1L, g2L ) )
	)
      )
    }    
   
    GenConGame(
      g1.left.zip( g2.left ).mapf( mComp ) ++ g1.right.zip( g2.right ).mapf( mComp ),
      g1.left.zip( g2.right ).mapf( mComp ) ++ g1.right.zip( g2.left ).mapf( mComp )
    )
  }
}
