// -*- mode: Scala;-*- 
// Filename:    LogicT.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 25 20:30:44 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.delimited

import com.biosimilarity.lift.lib.monad._

trait LogicT[T[M[_],_],M[_]] {
  self : MonadT[T,M] =>
    def msplit [M[_],A] ( tma : T[M,A] ) : T[M,Option[(A,T[M,A])]]
  def interleave [M[_],A] ( tma1 : T[M,A], tma2 : T[M,A] ) : T[M,A]
  def join [M[_],A,B] ( tma : T[M,A], binding : A => T[M,B] ) : T[M,B]
  def ifte [M[_],A,B] (
    tma : T[M,A], binding : A => T[M,B], tmb : T[M,B]
  ) : T[M,B]
  def once [M[_],A] ( tma : T[M,A] ) : T[M,A]
}

trait SFKTScope[M[_]] {
  type FK[Ans] = Ans
  type SK[Ans,A] = ( A, FK[Ans] ) => Ans

  type MonadM <: BMonad[M]
  def monadicMWitness : MonadM
  
  class SFKT[M[_],A](
    val unSFKT : ( SK[M[Any],A], FK[M[Any]] ) => M[Any]
  )
 
  object SFKT {
    def apply [M[_],A] (
      sk : ( SK[M[Any],A], FK[M[Any]] ) => M[Any]
    ) : SFKT[M,A] = {
      new SFKT[M,A]( sk )
    }
    def unapply [M[_],A] (
      sfkt : SFKT[M,A]
    ) : Option[( ( SK[M[Any],A], FK[M[Any]] ) => M[Any] )] = {
      Some( ( sfkt.unSFKT ) )
    }
  }
  
  case class SFKTC[A](
    override val unSFKT : ( SK[M[Any],A], FK[M[Any]] ) => M[Any]
  ) extends SFKT[M,A]( unSFKT )

  abstract class MonadicSKFTC
  extends BMonad[SFKTC] {
    override def unit [A] ( a : A ) : SFKTC[A] = {
      SFKTC( 
	{
	  ( sk : SK[M[Any],A], fk : FK[M[Any]] ) => {
	    sk( a, fk )
	  }
	}
      )
    }

    override def bind [A,B] (
      ma : SFKTC[A], 
      f : A => SFKTC[B]
    ) : SFKTC[B] =
      {
	val sfktc = 
	  {
	    ( sk : SK[M[Any],B], fk : FK[M[Any]] ) => {
	      val nsk = 
		{
		  ( a : A, nfk : FK[M[Any]] ) => {
		    ( f( a ).unSFKT )( sk, nfk )
		  }
		}	      
	      ( ma.unSFKT )( nsk, fk )	      
	    }
	  }
	SFKTC( sfktc )
      }    
  }
}


