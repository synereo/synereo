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
  // wrappers; they reflect some computation into a datum
  trait Tramp[A1] {
    def a : A1
    def flatMap [B] ( f : A1 => Shape[B] ) : Shape[B] = 
      bind[A1,B]( unit[A1]( a ), f )
    def foreach ( f : A1 => Unit ) : Unit = {
      fmap[A1,Unit]( f )( unit[A1]( a ) )
    }
    def map [B] ( f : A1 => B ) : Shape[B] = {
      fmap[A1,B]( f )( unit[A1]( a ) )
    }
  }
  class LazyTrampP[A1]( override val a : A1 ) extends Tramp[A1]
  object LazyTrampP {
    def apply [A1] ( a : A1 ) = new LazyTrampP( a )
    def unapply [A1] ( ltA : LazyTrampP[A1] ) : Option[( A1 )] = {
      Some( ( ltA.a ) )
    }
  }  
  class EagerTrampP[A1]( val sa : Shape[A1] ) extends Tramp[A1] {
    override def a : A1 = 
      throw new Exception( "can't generally get an a out" )
    override def flatMap [B]( f : A1 => Shape[B] ) : Shape[B] = 
      bind[A1,B]( sa, f )
    override def foreach ( f : A1 => Unit ) : Unit = {
      fmap[A1,Unit]( f )( sa )
    }
    override def map [B] ( f : A1 => B ) : Shape[B] = {
      fmap[A1,B]( f )( sa )
    }
  }
  object EagerTrampP {
    def apply [A1] ( sa : Shape[A1] ) = new EagerTrampP( sa )
    def unapply [A1] ( etA : EagerTrampP[A1] ) : Option[( Shape[A1] )] = {
      Some( ( etA.sa ) )
    }
  }  

  def toLTramp [A1] ( a : A1 ) : LazyTrampP[A1] = {
    LazyTrampP( a )
  }
  def toETramp [A1] ( sa : Shape[A1] ) : EagerTrampP[A1] = {
    EagerTrampP( sa )
  }  

  case class LazyTramp( override val a : A ) extends LazyTrampP[A]( a )
  case class EagerTramp( override val sa : Shape[A] ) extends EagerTrampP[A]( sa )

  def wrapLT ( a : A ) : LazyTramp = {
    LazyTramp( a )
  }
  def wrapET ( sa : Shape[A] ) : EagerTramp = {
    EagerTramp( sa )
  }  
  
}

trait SMonadT[T[M[_],_],M[_],A]
extends MonadT[T,M] {  
  trait TMSMA[A]
       extends ScalaMonadAdapter[TM,A]
       with BMonad[TM]
       with MonadPlus[TM]

  def tmsma : TMSMA[A]
}



