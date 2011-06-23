// -*- mode: Scala;-*- 
// Filename:    ListMonad.scala 
// Authors:     lgm                                                    
// Creation:    Fri May  6 23:54:47 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

class ListM[A]( )
extends ForNotationAdapter[List,A] 
with BMonad[List]
with MonadPlus[List]
with MonadFilter[List] {
  override def unit [S] ( s : S ) : List[S] = 
    List[S]( s )
  override def bind [S,T] (
    ls : List[S],
    f : S => List[T]
  ) : List[T] = {
    ( ( Nil : List[T] ) /: ls )( 
      {
	( acc, e ) => {
	  acc ++ f( e )
	}
      }
    )
  }
  override def zero [A] : List[A] = {
    List[A]( )
  }
  override def plus [A] (
    ma1 : List[A],
    ma2 : List[A]
  ) : List[A] = {
    ma1 ++ ma2
  }
  override def mfilter [S] (
    ls : List[S],
    pred : S => Boolean
  ) : List[S] = {
    ls.filter( pred )
  }
}
