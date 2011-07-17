// -*- mode: Scala;-*- 
// Filename:    FulcrumMonad.scala 
// Authors:     lgm                                                    
// Creation:    Sun Jul 17 08:13:40 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

import com.biosimilarity.lift.lib.comonad._
import com.biosimilarity.lift.lib.collection.Fulcrum
import com.biosimilarity.lift.lib.collection.FulcrumAlt2

trait FulcrumMonadScope[M[X] <: Iterable[X]] {
  def emptyM [X] : M[X]
  case class FulcrumC[A](
    override val _1 : A,
    override val _2 : M[Fulcrum[M,_]],
    override val _3 : A
  ) extends Fulcrum[M,A]( _1, _2, _3 )
  class FulcrumM[A]( )
  extends ForNotationAdapter[FulcrumC,A] 
  with BMonad[FulcrumC]
  with MonadFilter[FulcrumC] {
    override def unit [S] ( s : S ) : FulcrumC[S] = {
      FulcrumC[S]( s, emptyM[Fulcrum[M,_]], s )
    }
    override def bind [S,T] (
      fs : FulcrumC[S],
      f : S => FulcrumC[T]
    ) : FulcrumC[T] = {
      fs match {
	case FulcrumC( s1, lf, s2 ) => {
	  val FulcrumC( fs1t1, fs1lf, fs1t2 ) = f( s1 )
	  val FulcrumC( fs2t1, fs2lf, fs2t2 ) = f( s2 )
	  val nlf = ( fs1lf ++ lf ++ fs2lf ).asInstanceOf[M[Fulcrum[M,_]]]
	  FulcrumC( fs1t1, nlf, fs2t2 )
	}
      }
    }
    
    override def mfilter [S] (
      flcrm : FulcrumC[S],
      pred : S => Boolean
    ) : FulcrumC[S] = {
      flcrm match {      
	case FulcrumC( s1, lf, s2 ) => { 
	  if ( pred( s1 ) && pred( s2 ) ) {
	    FulcrumC( s1, lf, s2 )
	  }
	  else {
	    throw new Exception( "Maybe add option to S type in Fulcrim defn?" )
	  }
	}
      }
    }
  }

  class FulcrumCM[A]( )
  extends BComonad[FulcrumC] {
    override def counit [S] ( flcrm : FulcrumC[S] ) : S = {
      flcrm._3
    }
    override def cobind [S,T] (
      ctxt : FulcrumC[S] => T, 
      flcrm : FulcrumC[S] 
    ) : FulcrumC[T] = {
      val t : T = ctxt( flcrm )
      flcrm match {
	case FulcrumC( s1, lf, s2 ) => {	
	  FulcrumC( t, lf, t )
	}
      }
    }
  }
}

object FulcrumListM extends FulcrumMonadScope[List] {
  def emptyM [X] : List[X] = Nil
}

object FulcrumStreamM extends FulcrumMonadScope[Stream] {
  def emptyM [X] : Stream[X] = (List[X]( )).toStream
}
