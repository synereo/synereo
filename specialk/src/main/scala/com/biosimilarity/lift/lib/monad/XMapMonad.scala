// -*- mode: Scala;-*- 
// Filename:    XMapMonad.scala 
// Authors:     lgm                                                    
// Creation:    Sat Jul  2 21:33:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

import com.biosimilarity.lift.lib.collection.XMap
import com.biosimilarity.lift.lib.collection.RXMap

import scala.collection.SeqProxy
import scala.collection.immutable.HashMap

trait XMapMScope[A] {
  case class XMapC[B](
    override val seq : HashMap[A,B]
  ) extends XMap[A,B]( seq )  

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

trait XMapCMScope[B] {
  case class RXMapC[A](
    override val seq : HashMap[A,Either[B,RXMap[A,B]]]
  ) extends RXMap[A,B]( seq )

  case class RXASeq[A]( override val self : List[A] )
  extends SeqProxy[A]

  // class RXMapCM[A]( )
//   extends BComonad[RXMapC] {
//     override def counit [S] ( tS : RXMapC[S] ) : S = 
//       tS.attribute
//     override def cobind [S,T] (
//       ctxt : RXMapC[S] => T, 
//       tS : RXMapC[S] 
//     ) : RXMapC[T] = {
//       val t : T = ctxt( tS )
//       tS match {
// 	case tI@RXMapItem( _, attrItem ) => {
// 	  new RXMapItem( 
// 	    t,
// 	    attrItem
// 	  )
// 	}
// 	case tI@RXMapSection( _, attrSection ) => {
// 	  new RXMapSection( 
// 	    t,
// 	    attrSection.map(
// 	      ( atree : RXMapC[S] ) => {
// 		cobind( ctxt, atree )
// 	      }
// 	    )
// 	  )
// 	}
//       }
//     }
//   }
}
