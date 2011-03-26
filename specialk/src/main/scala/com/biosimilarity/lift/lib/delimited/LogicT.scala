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

