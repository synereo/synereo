// -*- mode: Scala;-*- 
// Filename:    Write.scala 
// Authors:     lgm                                                    
// Creation:    Wed Mar 12 21:54:45 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object WriterMonad {
  import MonadicEvidence._
  import MonadPlusEvidence._
  import FilteredMonadEvidence._
  
  case class HCtxt[C[_],A,Msg]( value : Option[A], record : C[Msg] )

  implicit def writerMonad[
    M[C[_]] <: Monad[C] with MonadPlus[C] with FilteredMonad[C],
    C[_],A
  ](
    implicit witness : M[C]
  ) : Monad[({ type L[Msg] = HCtxt[C,A,Msg]})#L] with MonadPlus[({ type L[Msg] = HCtxt[C,A,Msg]})#L] with FilteredMonad[({ type L[Msg] = HCtxt[C,A,Msg]})#L] = new Monad[({ type L[Msg] = HCtxt[C,A,Msg]})#L] with MonadPlus[({ type L[Msg] = HCtxt[C,A,Msg]})#L] with FilteredMonad[({ type L[Msg] = HCtxt[C,A,Msg]})#L]{
    def apply[Msg]( data : Msg ) = HCtxt[C,A,Msg]( None, witness.apply( data ) )
    def flatten[Msg]( hhv : HCtxt[C,A,HCtxt[C,A,Msg]] ) : HCtxt[C,A,Msg] = {
      val HCtxt( optA1 : Option[A], ch : C[HCtxt[C,A,Msg]] ) = hhv            
      var nrcrd : C[Msg] = witness.zero

      for( h <- monadToFilteredComprehension( witness( ch ) ) ) {
        val HCtxt( _ : A , rcrd : C[Msg] ) = h
        nrcrd = witness.plus[Msg]( nrcrd )( rcrd )
      }

      HCtxt( None, nrcrd )
    }    
    def zero[Msg] = HCtxt[C,A,Msg]( None, witness.zero )
    def plus[Msg]( h1 : HCtxt[C,A,Msg] )( h2 : HCtxt[C,A,Msg] ) : HCtxt[C,A,Msg] = {
      val ( HCtxt( optA1, rcrd1 ), HCtxt( optA2, rcrd2 ) ) = ( h1, h2 );
      HCtxt( None, witness.plus( rcrd1 )( rcrd2 ) )
    }
    def filter[Msg]( h : HCtxt[C,A,Msg] )( pred : Msg => Boolean ) : HCtxt[C,A,Msg] = {
      val HCtxt( optA, rcrd ) = h
      HCtxt( optA, witness.filter( rcrd )( pred ) )
    }
  }
}
