// -*- mode: Scala;-*- 
// Filename:    GenCon.scala 
// Authors:     lgm                                                    
// Creation:    Tue May 10 11:50:40 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game

import com.biosimilarity.lift.lib.monad._

import scala.collection.immutable.Map
import scala.collection.immutable.HashMap

trait GeneralizedConwayGame[+A] {
  def left : List[Either[A,GeneralizedConwayGame[A]]]
  def right : List[Either[A,GeneralizedConwayGame[A]]]
}

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

case object EmptyGenConGame
extends GeneralizedConwayGame[Nothing] {
  override def left : List[Either[Nothing,GeneralizedConwayGame[Nothing]]] = Nil
  override def right : List[Either[Nothing,GeneralizedConwayGame[Nothing]]] = Nil
}

case class GenConGame[+A](
  left : List[Either[A,GeneralizedConwayGame[A]]],
  right : List[Either[A,GeneralizedConwayGame[A]]]
) extends GeneralizedConwayGame[A] 

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
  def left : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( a ) )
  }
  def right : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Right( g ) )
  }
}
case class GCGSumRight[+A](
  g : GeneralizedConwayGame[A],
  a : A  
) extends GeneralizedConwayGame[A] 
     with GCGFormalSum[A]
{
  def left : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Right( g ) )
  }
  def right : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( a ) )
  }
}

// This class arises when we want to multiply games. What happens when we
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
  def left : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( a ) )
  }
  def right : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Right( g ) )
  }
}

case class GCGScalarMultipleRight[+A](
  g : GeneralizedConwayGame[A],
  a : A
) extends GeneralizedConwayGame[A] 
     with GCGScalarMultiple[A]
{
  def left : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Right( g ) )
  }
  def right : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( a ) )
  }
}

case class GCGScalarMultipleMiddle[+A](
  leftA : A,
  rightA : A
) extends GeneralizedConwayGame[A] 
     with GCGScalarMultiple[A]
{
  def left : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( leftA ) )
  }
  def right : List[Either[A,GeneralizedConwayGame[A]]] = {
    List( Left( rightA ) )
  }
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
		g1.left.map(
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
		g2.left.map(
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
		g1.right.map(
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
		g2.right.map(
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
    GenConGame( g.right, g.left )
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

    val lhmht =
      for(
	g1L <- g1.left; g1R <- g1.right;
	g2L <- g2.left; g2R <- g2.right
      ) yield {
	(
	  Right(
	    add(
	      add(
		timesLeft( g1L, g2 ),
		timesRight( g1, g2L )
	      ),
	      minus( timesMiddle( g1L, g2L ) )
	    )
	  ),
	  Right(
	    add(
	      add(
		timesLeft( g1R, g2 ),
		timesRight( g1, g2R )
	      ),
	      minus( timesMiddle( g1R, g2R ) )
	    )
	  ),
	  Right(
	    add(
	      add(
		timesLeft( g1L, g2 ),
		timesRight( g1, g2R )
	      ),
	      minus( timesMiddle( g1L, g2R ) )
	    )
	  ),
	  Right(
	    add(
	      add(
		timesLeft( g1R, g2 ),
		timesRight( g1, g2L )
	      ),
	      minus( timesMiddle( g1R, g2L ) )
	    )
	  )
	)
      }
    GenConGame(
      lhmht.map( _._1 ) ++ lhmht.map( _._2 ),
      lhmht.map( _._3 ) ++ lhmht.map( _._4 )
    )
  }
}

trait GenConPreM[A] {
  def gcgBind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    GenConGame(
      ( ( Nil : List[Either[T,GeneralizedConwayGame[T]]] ) /: ls.left )( 
	{
	  ( acc, e ) => {
	    acc ++ List(
	      Right(
		e match {
		  case Right( g ) => gcgBind( g, f )
		  case Left( s ) => f( s )
		}
	      )
	    )
	  }
	}
      ),
      ( ( Nil : List[Either[T,GeneralizedConwayGame[T]]] ) /: ls.right )( 
	{
	  ( acc, e ) => {
	    acc ++ List(
	      Right(
		e match {
		  case Right( g ) => gcgBind( g, f )
		  case Left( s ) => f( s )
		}
	      )
	    )
	  }
	}
      )
    )
  }
  def gcgMfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    GenConGame(
      ls.left.filter(
	( gl ) => {
	  gl match {
	    case Left( s ) => pred( s )
	    case Right( h ) => {
	      gcgMfilter( h, pred ) match {
		case EmptyGenConGame => false
		case _ => true
	      }
	    }
	  }
	}
      ),
      ls.right.filter(
	( gl ) => {
	  gl match {
	    case Left( s ) => pred( s )
	    case Right( h ) => {
	      gcgMfilter( h, pred ) match {
		case EmptyGenConGame => false
		case _ => true
	      }
	    }
	  }
	}
      )
    )
  }
}

class GenConLeftM[A]( )
extends ForNotationAdapter[GeneralizedConwayGame,A]
with BMonad[GeneralizedConwayGame]
with MonadFilter[GeneralizedConwayGame]
with GenConPreM[A] {
  override def unit [S] ( s : S ) : GeneralizedConwayGame[S] = 
    GenConGame[S]( List( Left[S,GeneralizedConwayGame[S]]( s ) ), Nil )
  override def bind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    gcgBind( ls, f )
  }
  override def mfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    gcgMfilter( ls, pred )
  }
}

class GenConRightM[A]( )
extends ForNotationAdapter[GeneralizedConwayGame,A]
with BMonad[GeneralizedConwayGame]
with MonadFilter[GeneralizedConwayGame]
with GenConPreM[A] {
  override def unit [S] ( s : S ) : GeneralizedConwayGame[S] = 
    GenConGame[S]( Nil, List( Left[S,GeneralizedConwayGame[S]]( s ) ) )
  override def bind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    gcgBind( ls, f )
  }
  override def mfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    gcgMfilter( ls, pred )
  }
}
