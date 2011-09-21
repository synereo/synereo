// -*- mode: Scala;-*- 
// Filename:    MonadAPI.scala 
// Authors:     lgm                                                    
// Creation:    Sat Mar 12 17:28:47 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

// How the category boyz be looking at things monadically
trait Monad[M[_]] {
  def fmap [A,B] ( f : A => B ) : M[A] => M[B]
  def unit [A] ( a : A ) : M[A]
  def mult [A] ( mma : M[M[A]] ) : M[A]  
  def bind [A,B] ( ma : M[A], f : A => M[B] ) : M[B] = {
    mult( fmap( f )( ma ) )
  }
}

// How the Haskell boyz be looking at things monadically
trait BMonad[M[_]] extends Monad[M] {
  override def fmap [A,B] (
      f : A => B
    ) : M[A] => M[B] = {
      ( ma : M[A] ) => {
	bind( ma, ( a : A ) => unit( f( a ) ) )
      }
    }
  override def mult [A] ( mma : M[M[A]] ) : M[A] = {
    bind[M[A],A]( mma, ( ma ) => ma )
  }
  override def bind [A,B] ( ma : M[A], f : A => M[B] ) : M[B]
}

// More monadically good richness
trait MonadPlus[M[_]] {
  self : Monad[M] =>
    def zero [A] : M[A]
  def plus [A] ( ma1 : M[A], ma2 : M[A] ) : M[A]
  def msum [A] ( lma : List[M[A]] ) : M[A] = {
    ( zero[A] /: lma )( plus )
  }
}

trait StrongMonad[M[_]] {
  self : Monad[M] =>
    def strength [P[_,_],A,B] ( pAMB : P[A,M[B]] ) : M[P[A,B]]
}

trait MonadFilter[M[_]] {
  self : Monad[M] =>
    def mfilter [A] ( ma : M[A], pred : A => Boolean ) : M[A]
}

// Still more monadically good richness
trait MonadT[T[M[_],_],M[_]] {
  //self : BMonad[M] =>
  // this is a hack
  type MonadM <: BMonad[M]
  type TM[A] <: T[M,A]
  type MonadTM <: BMonad[TM] with MonadPlus[TM]

  def monadicMWitness : MonadM  
  def monadicTMWitness : MonadTM  
  
  def lift [A] ( ma : M[A] ) : T[M,A] = {
    liftC[A]( ma )
  }
  def liftC [A] ( ma : M[A] ) : TM[A]
}

// From Atkey's paper
// Convenience conversions
trait ParametricMonad[M[_,_,_],S,T,U] {
  def inhabitant : M[S,U,T]
  def >>= [S1,V] (
    f : S => M[S1,V,U]
  )( implicit witness : PMonad[M] ) : M[S1,V,T] = {
    witness.bind[S,S1,T,U,V]( inhabitant, f )
  }
}

object ParametricMonad {
  implicit def paraMnd [M[_,_,_],A,B,C] (
    mabc : M[A,C,B]
  ) : ParametricMonad[M,A,B,C] =
    new ParametricMonad[M,A,B,C] {
      override val inhabitant : M[A,C,B] = mabc
    }
}

trait PMonad[M[_,_,_]] {
  def fmap [S1,S2,T,U] ( f : S1 => S2 ) : M[S1,T,U] => M[S2,T,U]
  def unit [S,T] ( s : S ) : M[S,T,T]
  def mult [S,T,U,V] ( mms : M[M[S,V,U],U,T] ) : M[S,V,T]
  def bind [S1,S2,T,U,V] (
    ms : M[S1,U,T],
    f : S1 => M[S2,V,U]
  ) : M[S2,V,T] = {
    mult( fmap( f )( ms ) )
  }
  def strength [S1,S2,T,U] ( s : S1, ms : M[S2,T,U] ) : M[( S1, S2 ),T,U]
}





