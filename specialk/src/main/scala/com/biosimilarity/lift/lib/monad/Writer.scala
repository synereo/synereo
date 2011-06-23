// -*- mode: Scala;-*- 
// Filename:    Writer.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jun 22 10:44:15 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

// An HistoricalContext is parametric in M, the Monad(Plus) that
// accumulates the logging messages, as well as the type, Msg, of
// logging messages
trait HistoricalContext[M[_],A,Msg]
extends BMonad[M] with MonadPlus[M] with MonadFilter[M] {
  def value : A
  def record : M[Msg]
}

// An HistoricalContextScope is needed to adjust the number of type
// parameters of the context to fit the MonadAPI
trait HistoricalContextScope[M[_],Msg] {
  trait MHCtxt[A] extends HistoricalContext[M,A,Msg]

  // These witness the fact that an HistoricalContext is actually a product
  implicit def toMHCtxt[A]( tpl : ( A, M[Msg] ) ) : MHCtxt[A]
  implicit def fromMHCtxt[A]( mhctxt : MHCtxt[A] ) : ( A, M[Msg] )

  // The WriterM is the witness that the HistoricalContext is actually
  // a monad itself
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

package usage {

/* ------------------------------------------------------------------------
 * A self-contained, in place example of instantiating the API for use
 * in an application.
 ------------------------------------------------------------------------- */

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

/*
 * A trace of the sample in usage running on Mac OS X Snow Leopard on
  2.8 GHz Intel Core i7
  *
Welcome to Scala version 2.9.0.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_24).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import com.biosimilarity.lift.lib.monad._
import com.biosimilarity.lift.lib.monad._
import com.biosimilarity.lift.lib.monad._

scala> import usage._
import usage._
import usage._

scala> import StringListLog._
import StringListLog._
import StringListLog._

scala> val sw = beginLogging( 5 )
val sw = beginLogging( 5 )
sw: com.biosimilarity.lift.lib.monad.usage.StringListLog.StringWriter[Int] = 
StringWriter(5,List(This (5) is a 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
Oh! a 'class java.lang.Integer'.
))

scala> import sw._
import sw._
import sw._

scala> for( a <- logValue( 6 ); b <- logValue( 7 ) ) 
for( a <- logValue( 6 ); b <- logValue( 7 ) ) 
     | yield { tell( "multiplying..." ); a * b }
yield { tell( "multiplying..." ); a * b }
multiplying...
res0: sw.Membrane[Int] = 
SCell(Wrapper(42,List(This (6) is a 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
Oh! a 'class java.lang.Integer'.
, This (7) is a 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
A what?
A 'class java.lang.Integer'.
Oh! a 'class java.lang.Integer'.
)))

scala> :q
*/
}
