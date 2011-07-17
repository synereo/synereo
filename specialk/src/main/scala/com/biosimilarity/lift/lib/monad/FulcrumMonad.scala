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

class FulcrumM[A]( )
extends ForNotationAdapter[Fulcrum,A] 
with BMonad[Fulcrum]
with MonadFilter[Fulcrum] {
  override def unit [S] ( s : S ) : Fulcrum[S] = 
    Fulcrum[S]( s, Nil, s )
  override def bind [S,T] (
    fs : Fulcrum[S],
    f : S => Fulcrum[T]
  ) : Fulcrum[T] = {
    fs match {
      case Fulcrum( s1, lf, s2 ) => {
	val Fulcrum( fs1t1, fs1lf, fs1t2 ) = f( s1 )
	val Fulcrum( fs2t1, fs2lf, fs2t2 ) = f( s2 )
	Fulcrum( fs1t1, ( fs1lf ++ lf ++ fs2lf ), fs2t2	)
      }
    }
  }

  override def mfilter [S] (
    flcrm : Fulcrum[S],
    pred : S => Boolean
  ) : Fulcrum[S] = {
    flcrm match {      
      case Fulcrum( s1, lf, s2 ) => { 
	if ( pred( s1 ) && pred( s2 ) ) {
	  Fulcrum( s1, lf, s2 )
	}
	else {
	  throw new Exception( "Maybe add option to S type in Fulcrim defn?" )
	}
      }
    }
  }
}

class FulcrumCM[A]( )
extends BComonad[Fulcrum] {
  override def counit [S] ( flcrm : Fulcrum[S] ) : S = {
    flcrm._3
  }
  override def cobind [S,T] (
    ctxt : Fulcrum[S] => T, 
    flcrm : Fulcrum[S] 
  ) : Fulcrum[T] = {
    val t : T = ctxt( flcrm )
    flcrm match {
      case Fulcrum( s1, lf, s2 ) => {	
	Fulcrum( t, lf, t )
      }
    }
  }
}
