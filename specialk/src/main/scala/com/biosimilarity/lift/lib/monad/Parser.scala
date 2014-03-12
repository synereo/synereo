// -*- mode: Scala;-*- 
// Filename:    Parser.scala
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 04:43:07 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object ParserMonad {
  import MonadicEvidence._
  trait ParserT[R,T] {
    def parse : Seq[T] => List[(R,Seq[T])]
  }
  class Parser[R,T](
    override val parse : Seq[T] => List[(R,Seq[T])]
  ) extends ParserT[R,T] {
    def apply( sT : Seq[T] ) : List[(R,Seq[T])] = parse( sT )
  }
  
  implicit def parserFunctor[T](
  ) : Functor[({type Lambda[R] = Parser[R,T]})#Lambda] =
    new Functor[({type Lambda[R] = Parser[R,T]})#Lambda] {
      def fmap[V, P >: V, U]( f : P => U ) : Parser[P,T] => Parser[U, T] = 
	( p : Parser[P,T] ) => 
	  new Parser[U, T](
            ( s ) => p( s ).map( { case ( r, ns ) => ( f( r ), ns ) } )
          )
    }

  // Note: we've picked up a proof obligation to show that the direct
  // implementation of fmap above is equivalent to the derived
  // implementation arising from the monadicity of Parser.
  implicit def parserMonad[T](
  ) : Monad[({type Lambda[R] = Parser[R,T]})#Lambda] =
    new Monad[({type Lambda[R] = Parser[R,T]})#Lambda] {      
      def apply[V]( data : V ) =
        new Parser(( s : Seq[T] ) => List( ( data, s ) ) )      
      def flatten[V]( m : Parser[Parser[V, T], T] ) : Parser[V, T] =
        new Parser[V, T](
          ( s ) => m( s ).flatMap( { case ( p, ns ) => p( ns ) } )
        )
    }  
}
