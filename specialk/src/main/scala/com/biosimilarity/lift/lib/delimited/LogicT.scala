// -*- mode: Scala;-*- 
// Filename:    LogicT.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 25 20:30:44 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.delimited

import com.biosimilarity.lift.lib.monad._

/* ------------------------------------------------------------------------
 *
 * The LogicT monad transformer provides an interface for handling
 * backtracking in any monad, M. It does so with a sensitivity to the
 * possibility of divergence of computations. Thus, it provides a fair
 * disjunction (interleaving) and conjunction (join).
 *
 * The interface is factored through one function, msplit. Thus, an
 * implementation is only required to provide the implementation of
 * that function.
 *
 * Practically, this differs from the paper by Kiselyov, Shan and
 * Friedman due to the differences between Haskell and Scala's version
 * of higher-kinded types. We have documented the differences in two
 * ways: 1) by adding comments regarding alternative signatures of the
 * functions making up the interface that would be more closely
 * aligned with a transliteration from the Haskell presentation; 2) by
 * marking the provided functions with a C for the type-level Currying
 * needed to make the Scala compiler happy.
 * 
 * ------------------------------------------------------------------------ */
trait LogicT[T[M[_],_],M[_],A] {
  //self : SMonadT[T,M,A] =>
  self : FNMonadT[T,M,A] =>
	  
    //def tracker : TMSMA[Option[(A,TM[A])]]

  // def msplit [M[_],A] ( tma : T[M,A] ) : T[M,Option[(A,T[M,A])]]
  // def msplit [A] ( tma : T[M,A] ) : T[M,Option[(A,T[M,A])]]

  def msplitC [A] ( tma : TM[A] ) : TM[Option[(A,TM[A])]]

  // def interleave [M[_],A] ( tma1 : T[M,A], tma2 : T[M,A] ) : T[M,A]
  // def interleave [A] ( tma1 : T[M,A], tma2 : T[M,A] ) : T[M,A]
  def interleaveC [A] ( tma1 : TM[A], tma2 : TM[A] ) : TM[A] = {
    monadicTMWitness.bind(
      msplitC( tma1 ),
      {
	( r : Option[(A,TM[A])] ) => {
	  r match {
	    case None => tma2
	    case Some( ( tma11, tma12 ) ) => {
	      monadicTMWitness.plus(
		monadicTMWitness.unit( tma11 ),
		interleaveC( tma2, tma12 )
	      )
	    }
	  }
	}
      }
    )
  }

  // def join [M[_],A,B] ( tma : T[M,A], binding : A => T[M,B] ) : T[M,B]
  // def join [A,B] ( tma : T[M,A], binding : A => T[M,B] ) : T[M,B]
  def joinC [A,B] ( tma : TM[A], binding : A => TM[B] ) : TM[B] = {
    monadicTMWitness.bind(
      msplitC( tma ),
      {
	( r : Option[(A,TM[A])] ) => {
	  r match {
	    case None => monadicTMWitness.zero
	    case Some( ( tma1, tma2 ) ) => {
	      interleaveC( binding( tma1 ), joinC( tma2, binding ) )
	    }
	  }
	}
      }
    )
  }

  // def ifte [M[_],A,B] (
  //   tma : T[M,A], binding : A => T[M,B], tmb : T[M,B]
  // ) : T[M,B]
  // def ifte [A,B] (
  //  tma : T[M,A], binding : A => T[M,B], tmb : T[M,B]
  // ) : T[M,B]
  def ifteC [A,B] (
    tma : TM[A], binding : A => TM[B], tmb : TM[B]
  ) : TM[B] = {
    monadicTMWitness.bind(
      msplitC( tma ),
      {
	( r : Option[(A,TM[A])] ) => {
	  r match {
	    case None => tmb
	    case Some( ( tma1, tma2 ) ) => {
	      monadicTMWitness.plus(
		binding( tma1 ),
		monadicTMWitness.bind( tma2, binding )
	      )
	    }
	  }
	}
      }
    )
  }

  //def once [M[_],A] ( tma : T[M,A] ) : T[M,A]
  //def once [A] ( tma : T[M,A] ) : T[M,A]
  def onceC [A] ( tma : TM[A] ) : TM[A] = {
    monadicTMWitness.bind(
      msplitC( tma ),
      {
	( r : Option[(A,TM[A])] ) => {
	  r match {
	    case None => monadicTMWitness.zero
	    case Some( ( tma1, _ ) ) => {
	      monadicTMWitness.unit( tma1 )
	    }
	  }
	}
      }
    )
  }
}

trait LogicTRunner[T[M[_],_],M[_],A] {
  //self : SMonadT[T,M,A] with LogicT[T,M,A] =>
  self : FNMonadT[T,M,A] with LogicT[T,M,A] =>   
    
  def bagOfNC( oN : Option[Int], tma : TM[A] ) : TM[List[A]] = {
    def bagOfNCP(
      oATMA : Option[(A,TM[A])]
    ) : TM[List[A]] = {
      oATMA match {
	case None => {
	  monadicTMWitness.unit( List() )
	}
	case Some( ( a, tmap ) ) => {
	  monadicTMWitness.bind(
	    bagOfNC(
	      for( n <- oN ) yield { n - 1 },
	      tmap
	    ),
	    ( lA : List[A] ) => {
	      monadicTMWitness.unit(
		List( a ) ++ lA
	      )
	    }
	  )
	}
      }
    }
    oN match {
      case Some( n ) => {
	if ( n <= 0 ) {
	  monadicTMWitness.unit( List() )
	}
	else {
	  monadicTMWitness.bind(
	    msplitC( tma ),
	    bagOfNCP
	  )
	}
      }
      case None => {
	monadicTMWitness.bind(
	    msplitC( tma ),
	    bagOfNCP
	  )
      }
    }
  }  
}

trait LogicTOps[T[M[_],_],M[_],A] 
extends LogicT[T,M,A]{
  //self : SMonadT[T,M,A] =>
	//with MonadPlus[M] =>
  self : FNMonadT[T,M,A] =>
    
  //def mplusTMWitness : MonadPlus[TM] with MonadM

  def reflect [A] ( optATMA : Option[(A,TM[A])] ) : TM[A] = {
    optATMA match {
      case None => monadicTMWitness.zero
      case Some( ( a, tma ) ) => {
	monadicTMWitness.plus(
	  monadicTMWitness.unit( a ).asInstanceOf[TM[A]],
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

  trait MonadicSFKTC
  extends BMonad[SFKTC]
       with MonadPlus[SFKTC]
       with MonadFilter[SFKTC]
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

    override def mfilter [A] (
      ma : SFKTC[A],
      pred : A => Boolean
    ) : SFKTC[A] = {
      SFKTC[A](
	{
	  ( sk : SK[M[Any],A], fk : FK[M[Any]] ) => {
	    val nsk = 
	      {
		( a : A, nfk : FK[M[Any]] ) => {
		  if ( pred( a ) ) {
		    ma.unSFKT( sk, nfk )
		  }
		  else {
		    fk
		  }
		}
	      }	      
	    ma.unSFKT( nsk, fk )
	  }
	}
      )
    }
  }
  
  trait MonadTransformerSFKTC[A]
  extends MonadicSFKTC
  with MonadT[SFKT,M] {
    override type TM[A] = SFKTC[A]
    override type MonadTM = MonadicSFKTC

    override def liftC [A] ( ma : M[A] ) : SFKTC[A] = {
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

  abstract class LogicTSFKTC[A]
  extends MonadTransformerSFKTC[A]
	  with LogicTRunner[SFKT,M,A]
	  with LogicTOps[SFKT,M,A]
	  //with SMonadT[SFKT,M,A]
          with FNMonadT[SFKT,M,A]
  {
    override type TM[A] = SFKTC[A]

    def ssk [A] ( a : A, fk : FK[M[Any]] ) = {      
      fk match {
	case mOATMA : M[Option[(A,TM[A])]] => {
	  // liftC( mOATMA ) : SFKTC[Option[(A,TM[A])]]
	  // reflect : Option[(A,TM[A])] => SFKTC[A]
	  // bind( liftC( mOATMA ), reflect ) : SFKTC[A]
	  monadicMWitness.unit(
	    Some(
	      (
		a,
		bind[Option[(A,TM[A])],A](
		  liftC[Option[(A,TM[A])]]( mOATMA ),
		  reflect
		)
	      )
	    )
	  ).asInstanceOf[M[Any]]
	}
	case _ => {
	  throw new Exception( "Any for universal quantification problem" )
	}
      }      
    }

    override def msplitC [A] (
      tma : SFKTC[A] 
    ) : SFKTC[Option[( A, SFKTC[A] )]] = {
      val fk : M[Option[( A, SFKTC[A] )]] =
	monadicMWitness.unit( None )
      tma.unSFKT( ssk, fk.asInstanceOf[M[Any]] ) match {
	case mOATMA : M[Option[(A, SFKTC[A])]] => {
	  liftC( mOATMA )
	}
	case _ => {
	  throw new Exception( "Any for universal quantification problem" )
	}
      }      
    }    

    def observe [A] (
      tma : SFKTC[A]
    ) : M[A] = {
      val fk : Any = 
	() => (throw new Exception( "no answer" ));
      
      val ans : M[Any] =	  
	tma.unSFKT(
	  {
	    ( a, fk ) =>{
	      monadicMWitness.unit( a )
	    }
	  },
	  monadicMWitness.unit( fk )
	) 
      
      monadicMWitness.bind(
	ans,
	{
	  ( any : Any ) => {
	    any match {
	      case a : A => monadicMWitness.unit( a )
	      case _ => throw new Exception( "no answer" ) 
	    }
	  }
	}
      )
    }
    
    //def runL [L[_]] ( oN : Option[Int], lA : L[A] ) : List[A]
    // def runM ( oN : Option[Int], tma : SFKTC[A] ) : M[List[A]] = {
//       ( oN, tma ) match {
// 	case ( None, SFKTC( unSFKT ) ) => {
// 	  val sk = 
// 	    {
// 	      ( a : A, fk : FK[M[Any]] ) => {
// 		monadicMWitness.bind(
// 		  fk,
// 		  ( x : List[A] ) => {
// 		    monadicMWitness.unit(
// 		      List( a ) ++ x
// 		    )
// 		  }
// 		)
// 	      }
// 	    }
// 	  unSFKT( sk, ( monadicMWitness.unit( Nil ) ) )
// 	}
// 	case ( Some( n ), SFKTC( unSFKT ) ) => {
// 	  if ( n <= 0 ) {
// 	    monadicMWitness.unit( Nil )
// 	  }
// 	  else {
// 	    def runMP( oAM : Option[( A, M[A] )] ) : M[List[A]] = {
// 	      oAM match {
// 		case None => {
// 		  monadicMWitness.unit( Nil )
// 		}
// 		case Some( ( a, tmap ) ) => {
// 		  monadicMWitness.bind(
// 		    runM( Some( n - 1 ), tmap ),
// 		    ( x : List[A] ) => {
// 		      monadicMWitness.unit(
// 			List( a ) ++ x
// 		      )
// 		    }
// 		  )
// 		}
// 	      }
// 	    }
// 	  }
// 	}
// 	case ( Some( 1 ), SFKTC( unSFKT ) ) => {
// 	  unSFKT(
// 	    {
// 	      ( a : A, fk : FK[M[Any]] ) => {
// 		monadicMWitness.unit( List( a ) )
// 	      }
// 	    },
// 	    monadicMWitness.unit( Nil )
// 	  )
// 	}
//       }
//     }

//     def observeC( tma : SFKTC[A] ) : M[A] = {
//       lazy val fA : FK[M[Any]] =
// 	throw new Exception( "no answer" )
//       tma.unSFKT(
// 	( a : A, fk : FK[M[Any]] ) => {
// 	  monadicMWitness.unit( a )
// 	},
// 	fA
//       ).asInstanceOf[M[A]]
//     }
  }
}


