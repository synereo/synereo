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
    
  //def msplit [M[_],A] ( tma : T[M,A] ) : T[M,Option[(A,T[M,A])]]
  def msplit [A] ( tma : T[M,A] ) : T[M,Option[(A,T[M,A])]]
  //def interleave [M[_],A] ( tma1 : T[M,A], tma2 : T[M,A] ) : T[M,A]
  def interleave [A] ( tma1 : T[M,A], tma2 : T[M,A] ) : T[M,A]
  //def join [M[_],A,B] ( tma : T[M,A], binding : A => T[M,B] ) : T[M,B]
  def join [A,B] ( tma : T[M,A], binding : A => T[M,B] ) : T[M,B]
  // def ifte [M[_],A,B] (
//     tma : T[M,A], binding : A => T[M,B], tmb : T[M,B]
//   ) : T[M,B]
  def ifte [A,B] (
    tma : T[M,A], binding : A => T[M,B], tmb : T[M,B]
  ) : T[M,B]
  //def once [M[_],A] ( tma : T[M,A] ) : T[M,A]
  def once [A] ( tma : T[M,A] ) : T[M,A]
  
}

trait LogicTOps[T[M[_],_],M[_]] 
extends LogicT[T,M]{
  self : MonadT[T,M] =>
    
  def monadicMWitness : BMonad[M]
  def mplusTMWitness [A] : MonadPlus[TM] with BMonad[M]

  def reflect [A] ( optATMA : Option[(A,TM[A])] ) : TM[A] = {
    optATMA match {
      case None => mplusTMWitness.zero
      case Some( ( a, tma ) ) => {
	mplusTMWitness.plus(
	  mplusTMWitness.unit( a ).asInstanceOf[TM[A]],
	  tma
	)
      }
    }
  }
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
  extends BMonad[SFKTC]
	   with MonadPlus[SFKTC]
  {
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

    def zero [A] : SFKTC[A] = {
      SFKTC(
	{
	  ( _ : SK[M[Any],A], fk : FK[M[Any]] ) => {
	    fk
	  }
	}
      )
    }
    
    def plus [A] (
      ma1 : SFKTC[A],
      ma2 : SFKTC[A]
    ) : SFKTC[A] = {
      SFKTC(
	{
	  ( sk : SK[M[Any],A], fk : FK[M[Any]] ) => {
	    ma1.unSFKT(
	      sk,
	      ma2.unSFKT( sk, fk )
	    )
	  }
	}
      )
    }
  }
  
  class MonadTransformerSFKTC
  extends MonadicSKFTC
  with MonadT[SFKT,M] {
    override def lift [A] ( ma : M[A] ) : SFKT[M,A] = {
      SFKTC( 
	{
	  ( sk : SK[M[Any],A], fk : FK[M[Any]] ) => {
	    monadicMWitness.bind(
	      ma,
	      {
		( a : A ) => {
		  sk( a, fk )
		}
	      }
	    )
	  }
	}
      )
    }
  }

  abstract class LogicTSFKTC
  extends MonadTransformerSFKTC
	   with LogicTOps[SFKT,M]
  {
    override type TM[A] = SFKTC[A]
    override def msplit [A](
      tma : SFKT[M,A] 
    ) : SFKT[M,Option[( A, SFKT[M,A] )]]//  = {
//       def ssk( a : A, fk : FK[M[Any]] ) : M[Option[( A, SFKT[M,A] )]] = {
// 	val ntma : SFKT[M,Option[( A, SFKT[M,A] )]] =
// 	  fk match {
// 	    case ofk : M[Option[( A, SFKT[M,A] )]] =>
// 	      lift( ofk )
// 	    case _ =>
// 	      throw new Exception(
// 		"this is what you get for using Any for universal quantification"
// 	      )
// 	  }
// 	monadicMWitness.unit(
// 	  Some(
// 	    (
// 	      a,
// 	      bind[Option[( A, SFKT[M,A] )], A](
// 		ntma.asInstanceOf[SFKTC[Option[( A, SFKT[M,A] )]]],
// 		reflect
// 	      )
// 	    )
// 	  )
// 	)
//       }
      
//       val nssk : ( A, FK[M[Any]] ) => M[Any] = {
// 	( a : A, fk : FK[M[Any]] ) =>
// 	  ssk( a, fk ).asInstanceOf[M[Any]]
//       }
	

//       lift(
// 	tma.unSFKT(
// 	  nssk,
// 	  monadicMWitness.unit[Option[( A, SFKT[M,A] )]](
// 	    None
// 	  ).asInstanceOf[M[Any]]
// 	)
//       ).asInstanceOf[SFKT[M, Option[( A, SFKT[M,A] )]]]
//     }
  }
}


