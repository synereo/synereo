// -*- mode: Scala;-*- 
// Filename:    ComonadAPI.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jun 28 05:07:05 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.comonad

// How the category boyz be looking at things monadically
trait Comonad[M[_]] {
  def fmap [A,B] ( f : A => B ) : M[A] => M[B]
  def counit [A] ( ma : M[A] ) : A
  def comult [A] ( mma : M[A] ) : M[M[A]]  
  def cobind [A,B] ( f : M[A] => B, ma : M[A] ) : M[B] = {
    fmap( f )( comult( ma ) )
  }
}

// How the Haskell boyz be looking at things monadically
trait BComonad[M[_]] extends Comonad[M] {
  override def fmap [A,B] (
      f : A => B
    ) : M[A] => M[B] = {
    ( ma : M[A] ) => {
      cobind(
	( ma : M[A] ) => {
	  f( counit( ma ) )
	},
	ma
      )
    }
  }
  override def comult [A] ( ma : M[A] ) : M[M[A]] = {
    cobind[A,M[A]]( ( ma ) => ma, ma )
  }
  override def cobind [A,B] ( f : M[A] => B, ma : M[A] ) : M[B]
}
