// -*- mode: Scala;-*- 
// Filename:    SMA.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jun 22 11:01:30 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

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

