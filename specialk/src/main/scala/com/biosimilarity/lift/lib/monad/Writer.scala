// -*- mode: Scala;-*- 
// Filename:    Writer.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jun 22 10:44:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

trait HistoricalContext[M[_],A,Msg]
extends BMonad[M] with MonadPlus[M] with MonadFilter[M] {
  def value : A
  def record : M[Msg]
}

trait HistoricalContextScope[M[_],Msg] {
  trait MHCtxt[A] extends HistoricalContext[M,A,Msg]

  implicit def toMHCtxt[A]( tpl : ( A, M[Msg] ) ) : MHCtxt[A]
  implicit def fromMHCtxt[A]( mhctxt : MHCtxt[A] ) : ( A, M[Msg] )

  trait WriterM[A]
  extends ForNotationAdapter[MHCtxt, A] 
  with BMonad[MHCtxt]
  with MonadFilter[MHCtxt] {
    def accWitness : MHCtxt[A]
    override def unit [S] ( s : S ) : MHCtxt[S] = {
      toMHCtxt( ( s, accWitness.zero ) )
    }
    override def bind [S,T] (
      sm : MHCtxt[S],
      f : S => MHCtxt[T]
    ) : MHCtxt[T] = {
      val ( s, ms ) = fromMHCtxt( sm )
      val ( t, mt ) = fromMHCtxt( f( s ) )
            
      toMHCtxt( ( t, accWitness.plus( ms, mt ) ) )
    }
    override def mfilter [S] (
      sm : MHCtxt[S],
      pred : S => Boolean
    ) : MHCtxt[S] = {
      val ( s, ms ) = fromMHCtxt( sm )
      // BUGBUG -- lgm : should we get rid of s if it doesn't pass
      // filter?
      if ( pred( s ) ) {
	sm
      }
      else {
	throw new Exception( "should have provided an Option type" )
      }
    }
    
    def tell( msg : Msg ) : M[Unit] 
  }
}

object StringListLog
       extends HistoricalContextScope[List,String]
{
  case class Wrapper[A](
    override val value : A,
    override val record : List[String]
  ) extends ListM[A] with MHCtxt[A]

  implicit def toMHCtxt[A](
    tpl : ( A, List[String] )
  ) : MHCtxt[A] = {
    Wrapper( tpl._1, tpl._2 ) 
  }
  implicit def fromMHCtxt[A](
    mhctxt : MHCtxt[A]
  ) : ( A, List[String] ) = {
    val Wrapper( v, r ) = mhctxt
    ( v, r )
  }

  def reinforceDescription[A]( a : A ) : String = {
    (
      "This (" + a + ") is a '"
      + a.asInstanceOf[AnyRef].getClass
      + "'.\n"
      + "A what?\n"
      + "A '"
      + a.asInstanceOf[AnyRef].getClass
      + "'.\n"
      + "A what?\n"
      + "A '"
      + a.asInstanceOf[AnyRef].getClass
      + "'.\n"
      + "Oh! a '"
      + a.asInstanceOf[AnyRef].getClass
      + "'.\n"
    )
  }

  case class StringWriter[A]( value : A, record : List[String] )
       extends WriterM[A]
  {
    def accWitness : MHCtxt[A] = {
      Wrapper( value, record )
    }
    def tell( msg : String ) : List[Unit] = {
      for( m <- List( msg ) ) yield {
	println( m )
      }
    }    
    def logValue[A]( a : A ) : Wrapper[A] = {
      Wrapper( a, List( reinforceDescription( a ) ) )
    }
  }

  def beginLogging[A]( a : A ) : StringWriter[A] = {
    StringWriter[A]( a, List( reinforceDescription( a ) ) )
  }
}
