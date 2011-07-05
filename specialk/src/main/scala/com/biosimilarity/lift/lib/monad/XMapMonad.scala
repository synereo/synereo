// -*- mode: Scala;-*- 
// Filename:    XMapMonad.scala 
// Authors:     lgm                                                    
// Creation:    Sat Jul  2 21:33:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

import com.biosimilarity.lift.lib.collection.XMap

import scala.collection.immutable.HashMap

trait XMapMScope[A] {
  case class XMapC[B](
    override val self : HashMap[A,B]
  ) extends XMap[A,B]( self )

  class XMapM[B](
    val unitKey : A
  ) extends ForNotationAdapter[XMapC,B]
  with BMonad[XMapC]
  with MonadPlus[XMapC]
  with MonadFilter[XMapC] {
    override def unit [S] ( s : S ) : XMapC[S] = {
      val uSelf : HashMap[A,S] =
	(
	  new HashMap[A,S]() ++ List( ( unitKey, s ) )
	).asInstanceOf[HashMap[A,S]]
      XMapC[S]( uSelf )
    }
    override def bind [S,T] (
      xmapS : XMapC[S],
      f : S => XMapC[T]
    ) : XMapC[T] = {
      val newSelf =
      ( ( new HashMap[A,T]() ) /: xmapS.self )(
	{
	  ( acc, kv ) => {
	    acc ++ f( kv._2 )
	  }
	}
      )
      XMapC[T]( newSelf )
    }
    override def zero [T] : XMapC[T] = {
      XMapC[T]( new HashMap[A,T]() )
    }
    override def plus [T] (
      xmapT1 : XMapC[T],
      xmapT2 : XMapC[T]
    ) : XMapC[T] = {
      ( xmapT1 ++ xmapT2 ).asInstanceOf[XMapC[T]]
    }
    override def mfilter [S] (
      xmapS : XMapC[S],
      pred : S => Boolean
    ) : XMapC[S] = {
      ( ( ( XMapC[S]( new HashMap[A,S]() ) ) ) /: xmapS.self )(
	{
	  ( acc : XMapC[S], kv : ( A, S ) ) => {
	    if ( pred( kv._2 ) ) {
	      ( acc + ( kv ) ).asInstanceOf[XMapC[S]]
	    }
	    else {
	      acc
	    }
	  }
	}
      )
    }
  }
}
