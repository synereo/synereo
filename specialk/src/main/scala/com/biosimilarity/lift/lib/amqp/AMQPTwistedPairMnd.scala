// -*- mode: Scala;-*- 
// Filename:    AMQPTwistedPairMnd.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 19 10:54:21 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import _root_.com.rabbitmq.client.{ Channel => RabbitChan, _}
import _root_.scala.actors.Actor

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import _root_.java.io.ObjectInputStream
import _root_.java.io.ObjectOutputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.io.ByteArrayOutputStream
import _root_.java.util.Timer
import _root_.java.util.TimerTask

trait AMQPTwistedPairScope[T] 
{
  self : AMQPBrokerScope[T] with MonadicDispatcherScope[T] =>

    def src : URI
  def trgt : URI

  def srcHost : String = src.getHost
  def trgtHost : String = trgt.getHost
  def srcPort : Int = src.getPort
  def trgtPort : Int = trgt.getPort
  
  case class TwistedQueuePair[T](
    srcQ : AMQPQueue[T],
    trgtQ : AMQPQueue[T]
  ) {
    def !( msg : T ) : Unit = { trgtQ ! msg }
  }
  
  class TwistedQueuePairM[A](
    val exchange : String,
    val routingKey : String
  ) extends MonadicJSONAMQPDispatcher[A]
  with WireTap
  with Journalist
  with MonadicAMQPDispatcher[A]
  with MonadicWireToTrgtConversion
  with SenderFactory[A]
  with ForNotationShiv[TwistedQueuePair,A] 
  with ForNotationApplyShiv[TwistedQueuePair,A]
  with BMonad[TwistedQueuePair]
  with MonadPlus[TwistedQueuePair]
  with MonadFilter[TwistedQueuePair] {    
    override type ForNotationTrampoline[A] = TwistedPairCell[A]
    case class TwistedPairCell[A](
      pair : TwistedQueuePair[A]
    ) extends SCell( pair ) {
      override def foreach ( f : A => Unit ) : Unit = {
	reset { for( msg <- pair.srcQ.dispatcher ) { f( msg ) } } ;
	()
      }
    }

    override def apply [S] (
      pair : TwistedQueuePair[S]
    ) : ForNotationTrampoline[S] = {
      TwistedPairCell[S]( pair )
    }

    override def tap [A] ( fact : A ) : Unit = {
      reportage( fact )
    }

    override def unit [S] ( s : S ) : TwistedQueuePair[S] = {
      val rslt = zero[S]
      spawn {
	reset { for( e <- rslt.trgtQ.sender ) { s } }
      }
      rslt
    }
    override def bind [S,T] (
      pairS : TwistedQueuePair[S],
      f : S => TwistedQueuePair[T]
    ) : TwistedQueuePair[T] = {
      val acc = zero[T]
      spawn {
	reset {
	  for( s <- pairS.srcQ.dispatcher ) {
	    val pairT = f( s )
	    spawn {
	      reset {
		for( t <- pairT.srcQ.dispatcher ) {
		  for( e <- acc.trgtQ.sender ) { t }
		}
	      }
	    }		
	  }
	}
      }
      acc
    }
    override def zero [A] : TwistedQueuePair[A] = {
      TwistedQueuePair[A](
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[theMDS.Wire]( factory, srcHost, srcPort, exchange ),
	  sender[theMDS.Wire]( srcHost, srcPort, exchange, routingKey )
	),
	AMQPQueue[A](
	  exchange,
	  routingKey,
	  theMDS.serve[theMDS.Wire]( factory, trgtHost, trgtPort, exchange ),
	  sender[theMDS.Wire]( trgtHost, trgtPort, exchange, routingKey )
	)
      )
    }
    override def plus [A] (
      pairA1 : TwistedQueuePair[A],
      pairA2 : TwistedQueuePair[A]
    ) : TwistedQueuePair[A] = {
      val pairA12 = zero[A]
      spawn {
	reset {
	  for( a1 <- pairA1.srcQ.dispatcher ) {
	    for( e <- pairA12.trgtQ.sender ) { a1 }
	  }
	}
      }
      spawn {
	reset {
	  for( a2 <- pairA2.srcQ.dispatcher ) {
	    for( e <- pairA12.trgtQ.sender ) { a2 }
	  }
	}
      }
      pairA12
    }
    override def mfilter [S] (
      pairS : TwistedQueuePair[S],
      pred : S => Boolean
    ) : TwistedQueuePair[S] = {
      val rslt = zero[S]
      spawn {
	reset {
	  for( s <- pairS.srcQ.dispatcher ) {
	    if ( pred( s ) ) {
	      for( e <- rslt.trgtQ.sender ) { s }
	    }
	  }
	}
      }
      rslt
    }

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : TwistedQueuePairM[A] => {
	  (
	    exchange.equals( that.exchange )
	    && routingKey.equals( that.routingKey )
	  )
	}
	case _ => false
      }
    }
    
    override def hashCode( ) : Int = {
      (	
	( 37 * exchange.hashCode )
	+ ( 37 * routingKey.hashCode )
      )
    }
  }
}

class AMQPStdTwistedPairScope[T](
  override val factory : ConnectionFactory,
  override val src : URI,
  override val trgt : URI
) extends AMQPTwistedPairScope[T]
with AMQPBrokerScope[T]
with MonadicDispatcherScope[T] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : AMQPStdTwistedPairScope[T] => {
	(
	  factory.equals( that.factory )
	  && src.equals( that.src )
	  && trgt.equals( that.trgt )
	)
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * factory.hashCode() )
      + ( 37 * src.hashCode() )
      + ( 37 * trgt.hashCode() )
    )
  }
}

object AMQPStdTwistedPairScope {
  def apply [T] (
    factory : ConnectionFactory,
    src : URI,
    trgt : URI
  ) : AMQPStdTwistedPairScope[T] = {
    new AMQPStdTwistedPairScope[T]( factory, src, trgt )
  }
  def unapply [T] (
    amqpStdTPS : AMQPStdTwistedPairScope[T]
  ) : Option[( ConnectionFactory, URI, URI )] = {
    Some( ( amqpStdTPS.factory, amqpStdTPS.src, amqpStdTPS.trgt ) )
  }
}

case class AMQPStdTPS[T](
  override val src : URI,
  override val trgt : URI
) extends AMQPStdTwistedPairScope[T](
  AMQPDefaults.defaultConnectionFactory, src, trgt
)

package usage {
  import scala.collection.mutable.HashMap
  import java.util.UUID  

  object AMQPTPSample extends UUIDOps {
    val defaultSrcHost = "10.0.1.5"
    val defaultTrgtHost = "10.0.1.9"

    val defaultQueueUUID = getUUID
    
    val srcSeed = scala.math.round( scala.math.random * 100 ).toInt
    val trgtSeed = scala.math.round( scala.math.random * 100 ).toInt
        
    def setupAndRunTest(
      parity : Boolean,
      srcHost : URI,
      trgtHost : URI,
      queueStr : String,
      msgCount : Int
    ) = {
      val scope = new AMQPStdTPS[Int]( srcHost, trgtHost )
      val qpM =	new scope.TwistedQueuePairM[Int]( queueStr, "routeroute" )

      val qtp = qpM.zero[Int]

      val msgMap = new HashMap[Int,Int]()

      def loop( count : Int ) : Unit = {
	println( "entering msg loop with count : " + count )
	count match {
	  case 0 => {
	    println( "Nothing to do." )
	  }
	  case i => {
	    if ( i < 0 ) {
	      throw new Exception(
		"Tsk, tsk, play fair, now... please keep msg counts positive!"
	      )
	    }
	    else {
	      println(
		"Waiting for a message on queue : " + queueStr
	      )
	      for( msg <- qpM( qtp ) ) {
		val mms = msgMap.size
		println(
		  (
		    "received msg number " + mms
		    + " with contents " + msg
		    + " on " + queueStr 
		  )
		)

		msgMap += ( ( (mms + 1), msg ) )

		if ( mms == count - 1 ) {
		  if ( !parity ) {
		    println(
		      "Ha! We have the last word with msg " + mms
		    )
		    qtp ! mms
		  }
		  println( "All " + count + " messages sent and received." )
		  println( "Conversation summary: " )
		  for( order <- 1 to count ) {
		    val msg = msgMap( order )
		    val prefix = "received " + msg + " "
		    val suffix = 
		      order match {
			case 1 => 1 + "st" + " "
			case 2 => 2 + "nd" + " "
			case 3 => 3 + "rd" + " "
			case _ => order + "th" + " "
		      }
		    println( prefix + suffix + "msg" )
		  }
		  println( "Test successful." )
		}
		else {
		  if ( mms < count ) {
		    println( "replying with msg " + mms )
		    qtp ! mms
		  }
		  else {
		    println( "received unexpected msg: " + msg )
		  }
		}
	      }
	    }
	  }
	}
      }
      
      if ( parity ) {
	println(
	  "sending initial msg " + srcSeed + " on queue " + queueStr
	)
	qtp ! srcSeed
      }

      loop( msgCount )
    }
    
  }

}
