// -*- mode: Scala;-*- 
// Filename:    Continuation.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep 19 09:24:56 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

class Continuation[+A,-B,+C]( val k : ( A => B ) => C ) {
  def apply( f : A => B ) : C = k( f )
  def map( f : A => B ) : C = this( f )
  def foreach( f : A => B ) : C = map( f )
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : Continuation[A,B,C] => {
	k.equals( that.k )
      }
      case _ => {
	false
      }
    }
  }
  override def hashCode( ) : Int = {
    37 * k.hashCode
  }
}
object Continuation {
  def apply [A,B,C] (
    k : ( A => B ) => C
  ) : Continuation[A,B,C] = {
    new Continuation[A,B,C]( k )
  }
  def unapply [A,B,C] (
    kc : Continuation[A,B,C]
  ) : Option[( ( A => B ) => C )] = {
    Some( ( kc.k ) )
  }
}  

class ContinuationM[X,Y,Z]( )
extends PMonad[Continuation]
with ForNotationPShiv[Continuation,X,Y,Z] 
with ForNotationApplyPShiv[Continuation,X,Y,Z] {
  override def fmap [S1,S2,T,U] (
    f : S1 => S2
  ) : Continuation[S1,T,U] => Continuation[S2,T,U] = {
    ( ctxt : Continuation[S1,T,U] ) => {	
      Continuation[S2,T,U](
	( s22t : S2 => T ) => {
	  for( s1 <- ctxt ) yield { s22t( f( s1 ) ) }
	}
      )
    }
  }
  override def unit [S,T] ( s : S ) : Continuation[S,T,T] = {
    Continuation[S,T,T]( ( k : S => T ) => k( s ) )
  }
  override def mult [S,T,U,V] (      
    kks : Continuation[Continuation[S,V,U],U,T]
  ) : Continuation[S,V,T] = {      
    Continuation[S,V,T]( 
      ( s2v : S => V ) => {
	kks.map(
	  ( csvu : Continuation[S,V,U] ) => {
	    csvu.map( s2v )
	  }
	)
      }
    )
  }
  override def strength [S1,S2,T,U](
    s1 : S1, cs2tu : Continuation[S2,T,U] 
  ) : Continuation[Tuple2[S1,S2],T,U] = {
    Continuation[Tuple2[S1,S2],T,U] (
      ( s1s22t : (Tuple2[S1,S2] => T) ) => {
	cs2tu.map(
	  ( s2 : S2 ) => {
	    val s1s2 : Tuple2[S1,S2] = ( s1, s2 )
	      s1s22t( s1s2 )
	  }
	)
      }
    )
  }
}

class DelimitedContinuation[X,Y,Z]( )
extends ContinuationM[X,Y,Z]( ) {
  def reset [A,B,C] (
    c : Continuation[A,A,B]
  ) : Continuation[B,C,C] = {
    Continuation[B,C,C](
      ( b2c2c : ( B => C ) ) => {
	b2c2c( c.map( ( a : A ) => a ) )
      }
    )
  }
  def shift [A,B,C,D,E] (
    a2cbcc2cdde : ( A => Continuation[B,C,C] ) => Continuation[D,D,E]
  ) : Continuation[A,B,E] = {
    Continuation[A,B,E](
      ( a2b : A => B ) => {
	a2cbcc2cdde(
	  ( a : A ) => {
	    unit[B,C]( a2b( a ) )
	  }
	).map( ( d : D ) => d )
      }
    )
  }
}

package usage {
  import scala.concurrent.{Channel => Chan, _}
  import scala.concurrent.cpsops._
  import scala.collection.immutable.Stream

  object TryDelC {
    val dc1 = new DelimitedContinuation[Int,Int,Int]()
    def plus31 = {
      dc1.reset[Int,Int,Int](
	dc1.fmap( ( x : Int ) => { 3 + x } )(
	  dc1.shift[Int,Int,Int,Int,Int](
	    ( c : Int => Continuation[Int,Int,Int] ) => {
	      Continuation[Int,Int,Int](
		( k : Int => Int ) => {
		  c( 0 ).map(
		    ( a : Int ) => {
		      c( 1 ).map( 
			( b : Int ) => {
			  k( a + b )
			}
		      )
		    }
		  )
		}
	      )
	    }
	  )	  
	)
      )
    }
    def plus32 = {
      dc1.reset[Int,Int,Int](
	dc1.fmap( ( x : Int ) => x )(
	  dc1.shift[Int,Int,Int,Int,Int](
	    ( c : Int => Continuation[Int,Int,Int] ) => {
	      Continuation[Int,Int,Int](
		( k : Int => Int ) => {
		  c( 0 ).map( k ) + c( 1 ).map( k )
		}
	      )
	    }
	  )
	)
      )
    }
  }
  
  object CBOne {
    // This simulates and event stream
    def randomStream [Src]( size : Int ) ( srcGen : () => Src ) : Stream[Either[Src,Boolean]] = {
      import scala.math._
      val rstrm =
	Stream.cons(
	  (
	    if ( random < ( 1.0 / ( ( random * size ).toInt + 1 )) ) {
	      if ( random > 0.5 ) {
		Right[Src,Boolean]( true )
	      }
	      else {
		Left[Src,Boolean]( srcGen() )
	      }
	    }
	    else {
	      Right[Src,Boolean]( false )
	    }
	  ),
	  randomStream[Src]( size + 1 )( srcGen )
	)
      rstrm
    }
    def randomStream [Src] ( srcGen : () => Src ) : Stream[Either[Src,Boolean]] = {
      randomStream[Src]( 0 )( srcGen )
    }

    // This simulates a callback mechanism on events in the event stream
    def registerCB [Src,Trgt] (
      srcSource : Stream[Either[Src,Boolean]]
    )(
      pwrtr : java.io.PrintWriter
    )( cb : Src => Trgt ) : Unit = {
      var srcSrc = srcSource
      def loop() : Unit = {
	(new java.lang.Thread() {
	  override def run() : Unit = {
	    srcSrc.head match {
	      case Left( src ) => {
		pwrtr.print( "<event>" + cb( src ) + "</event>" )
		srcSrc = srcSrc.drop( 1 )
		loop()
	      }
	      case Right( true ) => {
		srcSrc = srcSrc.drop( 1 )
		pwrtr.print( "<tick/>" )
		loop()
	      }
	      case _ => {
		pwrtr.print( "</time>" )
	      }
	    }
	  }	  
	}).run()
      }
      pwrtr.print( "<time>" )
      loop()
    }
    def registerCB [Src,Trgt](
      srcGen : () => Src
    )(
      pwrtr : java.io.PrintWriter
    )( cb : Src => Trgt ) : Unit = {      
      (new java.lang.Thread() {
	override def run() : Unit = {
	  registerCB[Src,Trgt](
	    randomStream[Src]( srcGen )
	  )( pwrtr )( cb )
	}
      }).run()
    }

    def delimitedCallBacks( pwrtr : java.io.PrintWriter )( cb : Int => Int ) : Continuation[Unit,Unit,Unit] = {
      val rstrm = randomStream[Int]( () => { ( scala.math.random * 1000 ).toInt } )
      val dc1 = new DelimitedContinuation[Int,Unit,Unit]()
      dc1.reset[Int,Unit,Unit](
	dc1.fmap(
	  ( x : Int ) => {
	    val xp3 = x + 3
	    pwrtr.print( "<xform>3 + " + x + " = " + xp3 + "</xform>" )
	    xp3
	  }
	)(
	  dc1.shift[Int,Int,Unit,Unit,Unit](
	    ( c : Int => Continuation[Int,Unit,Unit] ) => {
	      Continuation[Unit,Unit,Unit](
		( k : Unit => Unit ) => {
		  val kcb : Int => Unit = {		    
		    ( i : Int ) => {
		      (new java.lang.Thread() {
			override def run() : Unit = {
			  pwrtr.print( "<cbScope>" )
			  c( i ).map(
			    ( j : Int ) => {
			      pwrtr.print(
				"<cbResults>" + cb( j ) + "</cbResults>"
			      )
			    } 
			  )
			  pwrtr.print( "</cbScope>" )
			  k()
			}
		      }).run()
		    }
		  }
		  pwrtr.print( "<registerCB>" )
		  registerCB[Int,Unit]( rstrm )( pwrtr )( kcb )
		  pwrtr.print( "</registerCB>" )
		}
	      )
	    }
	  )	  
	)
      )
    }

    def delimitedCallBacks( cb : Int => Int ) : scala.xml.Elem = {
      val sw = new java.io.StringWriter()
      val pw = new java.io.PrintWriter( sw )    
      delimitedCallBacks( pw )( cb )( Unit => { println( "done" ) } )
      scala.xml.XML.loadString( sw.toString )
    }
  }
}
