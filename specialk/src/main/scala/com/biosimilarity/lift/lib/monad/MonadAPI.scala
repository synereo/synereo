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
}

// Still more monadically good richness
trait MonadT[T[M[_],_],M[_]] {
  //self : BMonad[M] =>
  // this is a hack
  type MonadM <: BMonad[M]
  type TM[A] <: T[M,A]
  type MonadTM <: BMonad[TM]

  def monadicMWitness : MonadM  
  def monadicTMWitness : MonadTM  
  
  def lift [A] ( ma : M[A] ) : T[M,A]
  def liftC [A] ( ma : M[A] ) : TM[A]
}

// How the scala boyz be looking at things monadically
// and how that relates to what the category boyz be talkin 'bout
trait ScalaMonadAdapter[Shape[_],A] {
  self : Monad[Shape] =>
    def map [B] ( f : A => B ) : Shape[A] => Shape[B] = {
      fmap[A,B]( f )
    }
  def flatMapC [B]( sa : Shape[A] )( f : A => Shape[B] ) : Shape[B] = {
      bind[A,B]( sa, f )
    }
  def foreachC ( sa : Shape[A] )( f : A => Unit ) : Unit = {
    fmap( f )( sa );
    ()
  }
  def wrap ( a : A ) : Shape[A] = {
    unit[A]( a )
  }
  def roll ( ssa : Shape[Shape[A]] ) : Shape[A] = {
    mult [A] ( ssa )
  }

  // i can haz a witness?
  // These two classes show up cause all monads are effectively
  // wrappers; the reflect some computation into a datum
  case class LazyTramp( a : A ) {
    def flatMap [B] ( f : A => Shape[B] ) : Shape[B] = 
      flatMapC( wrap( a ) )( f )
    def foreach ( f : A => Unit ) : Unit = 
      foreachC( wrap( a ) )( f )
    def map [B] ( f : A => B ) : Shape[B] = {
      fmap( f )( wrap( a ) )
    }
  }
  case class EagerTramp( sa : Shape[A] ) {
    def flatMap [B]( f : A => Shape[B] ) : Shape[B] = 
      flatMapC( sa )( f )
    def foreach ( f : A => Unit ) : Unit = 
      foreachC( sa )( f )
    def map [B] ( f : A => B ) : Shape[B] = {
      fmap( f )( sa )
    }
  }

  def wrapLT ( a : A ) : LazyTramp = {
    LazyTramp( a )
  }
  def wrapET ( sa : Shape[A] ) : EagerTramp = {
    EagerTramp( sa )
  }  
  
}


